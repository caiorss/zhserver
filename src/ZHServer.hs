{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

{-
Description: Zhserver - Zotero/based Web server main file.
File:        ZHServer.hs  

-}


import Control.Monad.Trans (liftIO, lift)
import Control.Monad.Trans.Reader 
import Control.Monad.Trans.Maybe


import Data.Text        (Text)
import qualified Data.Text as T

import Control.Monad    

import Text.Read (readMaybe)

import System.Environment
import System.Exit

import qualified Happstack.Server.Response as Response

import qualified Happstack.Server.Internal.Types as ServerTypes 

import Happstack.Server (FromReqURI (..), dir, Conf, Conf (..), nullConf, ok
                         , seeOther, simpleHTTP, dir, dirs, path, seeOther, method
                         , Method (GET, POST, HEAD)
                         , ServerPart, ServerPartT, look
                         , flatten, toResponse 
                        
                        )

import Happstack.Server ( Browsing(EnableBrowsing, DisableBrowsing),
                          nullConf
                        , serveDirectory
                        , simpleHTTP
                        )

import Happstack.Server.FileServe ( asContentType
                                   ,mimeTypes
                                   ,serveFile
                                  )       

import qualified Data.ByteString.Lazy as BL 
import qualified Data.ByteString.Lazy.Char8 as LC 

import qualified Database.HDBC as HDBC
import qualified Database.HDBC.PostgreSQL as Pg
import qualified Database.HDBC.Sqlite3    as Sqlite3 

import Text.Printf (printf)

import qualified Zotero as Z

import Zotero (DBConn)

{-
   ReaderT conn (ServerPartT IO) response
    = conn -> (ServerPartT IO) response
    = conn -> ServerPartT IO response

-}
type ServerApp a =  forall conn. (HDBC.IConnection conn)
                    => ReaderT conn (ServerPartT IO) a

-- Database Connection - Objective make the database connection implementation agnostic.
--
data HDBConn =  HDBConnSqlite   Sqlite3.Connection
              | HDBConnPostgres Pg.Connection
              -- deriving (Eq, Read, Show)

-- Database URI
--
data DBUri = DBUriSqlite   String
           | DBUriPostGres String
           deriving (Eq, Read, Show)

data ServerConfig = ServerConfig
                    {
                     serverPort        :: Int      -- Port that server will listen
                    ,serverHost        :: String   -- Host that server will listen
                    ,serverStaticPath  :: String    -- Single Page App static files like /index.html, /js/script.js
                    ,serverStoragePath :: String   -- Zotero storage Path
                    ,serverDatabase    :: String   -- Server Database URI
                    } deriving (Eq, Show, Read)




serverConf :: Conf
serverConf = Conf
  { port      = 8080
  , validator = Nothing
  , logAccess = Nothing
  , timeout   = 30
  , threadGroup = Nothing
  }



parseInt :: String -> Maybe Int 
parseInt s = readMaybe s



parseDbDriver2 dbUri =
  case getDbType dbUri of
    "sqlite"    -> Just (DBUriSqlite   sqlitePath)
    "postgres"  -> Just (DBUriPostGres dbUri)
    _           -> Nothing
  where
    sqlitePath = (stripPrefix "sqlite://" dbUri)
    getDbType dbUri = T.unpack . (!!0) . T.split (==':') . T.pack $ dbUri


openDBConnection :: String -> IO (Maybe HDBConn)
openDBConnection dbUri =
  case parseDbDriver2 dbUri of
    Just (DBUriSqlite   uri) -> Sqlite3.connectSqlite3  uri >>= \conn -> return $ Just (HDBConnSqlite conn)
    Just (DBUriPostGres uri) -> Pg.connectPostgreSQL    uri >>= \conn -> return $ Just ( HDBConnPostgres conn)
    Nothing                  -> return Nothing


withConnServerDB ::  Response.ToMessage a =>
                     String         -- Database URI
                  -> Conf           -- Server Configuration
                  -> ServerApp a    -- Server Monad
                  -> IO ()
withConnServerDB dbUri conf serverApp = do
  maybeConn <- openDBConnection dbUri

  case maybeConn of

    Just (HDBConnSqlite conn)   ->  withConn conn conf serverApp
    Just (HDBConnPostgres conn) ->  withConn conn conf serverApp
    Nothing                     ->  putStrLn $ "Error: Invalid database URI " ++ dbUri

  where
    withConn conn conf serverApp = do
      -- Listen http request
      simpleHTTP conf $ runReaderT serverApp conn
      -- Disconnect database
      HDBC.disconnect conn
      return ()


withConnServer ::  (HDBC.IConnection conn, Response.ToMessage a) =>
                 (String -> IO conn)
                 -> String
                 -> Conf 
                 -> ServerApp a
                 -> IO ()                
withConnServer driver uri conf serverApp = do
  -- Open database Connection
  conn     <- driver  uri
  -- Listen http request
  simpleHTTP conf $ runReaderT serverApp conn
  -- Disconnect database
  HDBC.disconnect conn
  return ()

{-  Database connection is closed on each request -}
withConnServer2 ::  (HDBC.IConnection conn, Response.ToMessage a) =>
                 (String -> IO conn)  -- driver : Function that takes a database URI and returns a DB connection
                 -> String            -- uri    : Database URI/URL
                 -> Conf              -- conf   : Server Configuration
                 -> ServerApp a
                 -> IO ()  
withConnServer2 driver uri conf serverApp = do
  conn1     <- driver  uri
  simpleHTTP conf $ runReaderT serverWrapper conn1
  return ()

    where
--      serverWrapper :: ServerApp b
      serverWrapper = do
        conn      <- ask        
        response  <- serverApp
        liftIO    $ HDBC.disconnect conn
        return response
           


{- ========== Server Higher Order Functions ============ -}

runDbQuery :: DBConn b -> ServerApp b
runDbQuery dbQuery = do
  conn   <- ask
  result <- liftIO $ runReaderT dbQuery conn
  ReaderT (\conn -> return result)

{- Creates a sever route /path?<param>=<value>

-}
serverRouteParam :: (String -> Maybe a)  -- Parser Function 
                    -> String            -- Parameter Name
                    -> b                 -- Error Value 
                    -> (a -> DBConn b)   -- Database query 
                    -> ServerApp b                    
serverRouteParam parser paramName err dbFn = do
  param      <- look paramName
  let param' = parser param
  maybe (return err) (\p -> runDbQuery $ dbFn p) param'

{- Crates a server route for which the paramter is 
   an ID (int) - Identification Number for the database query 
-}
serverRouteParamID :: String
              -> (Int -> DBConn LC.ByteString)
              -> ServerApp LC.ByteString
serverRouteParamID param dbFn = 
  serverRouteParam parseInt param LC.empty dbFn


{- Crates a server route for which the paramter is 
   an ID - Identification Number for the database query 
-}
serverRouteParamString :: String
              -> (String -> DBConn LC.ByteString)
              -> ServerApp LC.ByteString
serverRouteParamString param dbFn = 
  serverRouteParam return param LC.empty dbFn


{-- ================ Server Routes Dispatch ========================== -}

makeRoutes :: String -> String -> ServerApp ServerTypes.Response
makeRoutes staticPath storagePath = msum

  [
    -- 
    flatten $ dir "api" $ dir "item" $  serverRouteParamID "id" Z.getZoteroItemJSON
    
    {- REST API -}


  , flatten $ dir "api" $ dir "collsw"  $ routeItemsWithoutCollection
          
    -- Return all items from a given collection
    -- 
    -- /api/colls?id=23423
    -- /api/coll?id={collection ID}
    --
    ,flatten $ dir "api" $ dir "colls" $ routeCollectionID

    -- Returns all collections 
    --
    -- /api/colls
    --

    --  End point: http://localhost:8000/api/colls
    -- 
    , flatten $ dir "api" $ dir "colls" $ routeCollection
    
    -- End point: http://localhost:8000/api/colls?id=6
    --            http://localhost:8000/api/colls?id=<collection ID>
    --
    , flatten $ dir "api" $ dir "colls" $ routeTagsFromCollID


    -- Returns all collections from a given tag
    --
    -- End Point: http://localhost:8000/api/tags?id=15
    --            http://localhost:8000/api/tags?id=<tag ID>
    --
   , flatten $ dir "api" $ dir "tags" $ routeTagID
      
   -- Returns all items from a given tag
   -- 
   -- End Point: http://localhost:8000/api/tags
  , flatten $ dir "api" $ dir "tags" $ routeTags

  -- Returns all items from a given author
  --
  -- /api/authors?id=34
  --
  , flatten $ dir "api" $ dir "authors" $ routeAuthorID

    
  , flatten $ dir "api" $ dir "authors" $ routeAuthors


  , flatten $ dir "api" $ dir "relatedtags" $ routeRealatedTags                 


  , flatten $ dir "api" $ dir "search" $ routeSearchByTitleLike
    
  , flatten $ dir "api" $ dir "search" $ routeSearchByContentAndTitleLike 
   
    
    -- Zotero Attachment Files
  , flatten $ dir "attachment" $ serveDirectory DisableBrowsing [] storagePath


  -- Single Page App static files
  , flatten $ serveDirectory DisableBrowsing     ["index.html",
                                                   "style.css",
                                                   "loader.js"
                                                  ]
                                                  staticPath
                                                  
 -- , flatten $ seeOther "static" "/"
  , flatten $ Response.notFound $ "Error: Not found"                                              
    
  ]


stripPrefix prefix str =
 case T.stripPrefix (T.pack prefix) (T.pack str) of
   Just s   -> T.unpack s
   Nothing  -> str


parseDbDriver uri =
  T.unpack . (!!0) . T.split (==':') . T.pack $ uri
  

-- Start server with a given configuration 
-- 
runServerConf :: ServerConfig -> IO ()
runServerConf conf = do
  let dbUri       = serverDatabase conf
  let port        = serverPort conf
  let storagePath = serverStoragePath conf
  let staticPath  = serverStaticPath conf
  let sconf       = Conf port Nothing Nothing 30 Nothing
  withConnServerDB dbUri sconf (makeRoutes staticPath storagePath)


runServer host port dbUri staticPath storagePath =
  case readMaybe port :: Maybe Int of
    Just p    -> do  let sconf = Conf p Nothing Nothing 30 Nothing
                     withConnServerDB dbUri sconf (makeRoutes staticPath storagePath)
    Nothing   -> putStrLn "Error: Invalid port number"

  
-- Start server loading its settings from a configuration file.
--
loadServerConf configFile = do
  conf' <- (\text -> readMaybe text :: Maybe ServerConfig) <$> readFile configFile
  case conf' of
    Just conf -> runServerConf conf
    Nothing   -> putStrLn "Error: failed parse the config file"



{- ================ HTTP ROUTES ======================== -}


routeCollection :: ServerApp LC.ByteString
routeCollection = runDbQuery Z.getCollectionsJSON

routeCollectionID :: ServerApp LC.ByteString 
routeCollectionID = serverRouteParamID "id" Z.getCollectionItemsJSON


routeTagID :: ServerApp LC.ByteString
routeTagID = serverRouteParamID "id" Z.getTagItemsJSON

  
routeTags :: ServerApp LC.ByteString
routeTags = runDbQuery  Z.getTagsJSON

routeAuthorID :: ServerApp LC.ByteString
routeAuthorID = serverRouteParamID "id"  Z.getItemsFromAuthorJSON
  
routeAuthors :: ServerApp LC.ByteString
routeAuthors = runDbQuery Z.getAuthorsJSON


routeTagsFromCollID :: ServerApp LC.ByteString
routeTagsFromCollID = serverRouteParamID "col2tag" Z.getTagsFromCollectionJSON


routeSearchByTitleLike :: ServerApp LC.ByteString
routeSearchByTitleLike =
  serverRouteParamString "title" Z.searchByTitleWordLikeJSON

routeRealatedTags :: ServerApp LC.ByteString
routeRealatedTags = serverRouteParamID "id" Z.getRelatedTagsJSON  

routeSearchByContentAndTitleLike :: ServerApp LC.ByteString
routeSearchByContentAndTitleLike =
  serverRouteParamString "content" Z.searchByContentAndTitleLikeJSON


routeItemsWithoutCollection :: ServerApp LC.ByteString
routeItemsWithoutCollection = do
  conn <- ask 
  paging <- parseInt <$>  look   "paging"
  offset <- parseInt <$>  look "offset"

  case paging of
    Nothing -> return (LC.pack "Error wrong parameters")

    Just p -> case offset of
              Nothing -> return (LC.pack "Error wrong parameters")
              Just o ->  runDbQuery $ Z.itemsWithoutCollectionsJSON p o



routeItemsWithoutCollection2 :: ServerApp LC.ByteString
routeItemsWithoutCollection2 = do 
  paging <- parseInt <$>  look "paging" -- :: Maybe Int 
  offset <- parseInt <$>  look "offset"   -- :: Maybe Int 
  let ans = do
        page <- paging 
        offs <- offset
        return (page, offs)        
  case ans of
    Nothing       -> return (LC.pack "Error wrong parameters")
    Just (p, o)   -> runDbQuery $ Z.itemsWithoutCollectionsJSON p o
    
  -- case paging of
    
  --   Nothing -> return (LC.pack "Error wrong parameters")

  --   Just p -> case offset of
  --             Nothing -> return (LC.pack "Error wrong parameters")
  --             Just o ->  runDbQuery $ Z.itemsWithoutCollectionsJSON p o



{- ==================== MAIN  ======================== -}


--  Server command line switches.
--
parseArgs :: [String] -> IO ()
parseArgs args =
  case args of
  []                 ->  do
                           putStrLn "Loading default configuration file from ZHSERVER_CONFIG environment variable."
                           conf  <- lookupEnv "ZHSERVER_CONFIG"
                           case conf of
                             Just file -> loadServerConf file
                             Nothing   -> do putStrLn "Error: Configuration file not found"

  ["--conf", file]   -> loadServerConf file
  ["-c", file]       -> loadServerConf file

  ["--params", host, port, dbUri, staticPath, storagePath] -> runServer host port dbUri staticPath storagePath

  -- ["-listen", host; port; "--dburi"; uri; "--storage"; path]  -> loadServerConf file
  
  _                  -> putStrLn "Error: Invalid option."



main = do
  putStrLn "Server Running"
  putStrLn "------------------"
  -- getArgs >>= \args -> putStrLn (show args)

  getArgs >>= parseArgs
  
  -- loadServerConf "zhserver.conf"

  -- withConnServer2 Pg.connectPostgreSQL
  --                "postgres://postgres@localhost/zotero"
  --                serverConf
  --                routes

  -- withConn Pg.connectPostgreSQL
  --          "postgres://postgres@localhost/zotero"
          
