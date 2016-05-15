{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}



import Control.Monad.Trans (liftIO, lift)
import Control.Monad.Trans.Reader 
import Control.Monad.Trans.Maybe

import Data.Text        (Text)
import qualified Data.Text as T

import Control.Monad    (msum, mapM, mapM_, foldM)

import Text.Read (readMaybe)

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

import qualified ConfParser as CFP

import Zotero (DBConn)


data HDBConn =   ConnSqlite Sqlite3.Connection
               | ConnPg     Pg.Connection
             

{-
   ReaderT conn (ServerPartT IO) response
    = conn -> (ServerPartT IO) response
    = conn -> ServerPartT IO response

-}
type ServerApp a =  forall conn. (HDBC.IConnection conn)
                    => ReaderT conn (ServerPartT IO) a


data ServerConfig = ServerConfig
                    {
                      serverPort        :: Int,
                      serverHost        :: String,
                      serverStoragePath :: String, 
                      serverDatabase    :: String

                    } deriving (Eq, Show, Read)

readInt :: String -> Maybe Int 
readInt s = readMaybe s


withConnServer ::  (HDBC.IConnection conn, Response.ToMessage a) =>
                 (String -> IO conn)
                 -> String
                 -> Conf 
                 -> ServerApp a
                 -> IO ()                
withConnServer driver uri conf serverApp = do
  conn     <- driver  uri
  simpleHTTP conf $ runReaderT serverApp conn
  HDBC.disconnect conn
  return ()

{-  Database connection is closed on each request -}
withConnServer2 ::  (HDBC.IConnection conn, Response.ToMessage a) =>
                 (String -> IO conn)
                 -> String
                 -> Conf 
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
  serverRouteParam readInt param LC.empty dbFn


{- Crates a server route for which the paramter is 
   an ID - Identification Number for the database query 
-}
serverRouteParamString :: String
              -> (String -> DBConn LC.ByteString)
              -> ServerApp LC.ByteString
serverRouteParamString param dbFn = 
  serverRouteParam return param LC.empty dbFn


routes :: ServerApp ServerTypes.Response
routes = msum

  [

    flatten $ dir "api" $ dir "item" $  serverRouteParamID "id" Z.getZoteroItemJSON
    
    {- REST API -}

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
    , flatten $ dir "api" $ dir "colls" $ routeCollection


    , flatten $ dir "api" $ dir "colls" $ routeTagsFromCollID

    -- Returns all collections from a given tag
    --
    -- /api/tags?id=343  
    --
   , flatten $ dir "api" $ dir "tags" $ routeTagID
      
   -- Returns all items from a given tag
   --
   -- /api/tags
   --
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
   
    
  {------------- Static Files -----------}
    
  , flatten $ dir "attachment" $ serveDirectory DisableBrowsing [] Z.storagePath
      
  , flatten $ dir "static" $  serveDirectory EnableBrowsing
                                                  ["index.html",
                                                   "style.css",
                                                   "loader.js"
                                                  ]
                                                  "."

  , flatten $ dir "arch" $ serveDirectory EnableBrowsing [] "/tmp/arch"
    
                                                  
  , flatten $ seeOther "static" "static"                                                       
    
  ]




serverConf :: Conf 
serverConf = Conf
  { port      = 8080
  , validator = Nothing
  , logAccess = Nothing
  , timeout   = 30
  , threadGroup = Nothing
  }

  
stripPrefix prefix str =
 case T.stripPrefix (T.pack prefix) (T.pack str) of
   Just s   -> T.unpack s
   Nothing  -> str

parseDbDriver uri =
  T.unpack . (!!0) . T.split (==':') . T.pack $ uri
   
-- openDBConnection :: HDBC.IConnection conn => String -> IO HDBConn
-- openDBConnection uri = do
--   let driver = T.unpack . (!!0) . T.split (==':') . T.pack $ uri

--   case driver of
--     "sqlite"   -> ConnSqlite <$>  Sqlite3.connectSqlite3 (stripPrefix "sqlite://" uri)
--     "postgres" -> ConnPg <$> Pg.connectPostgreSQL uri
--     _          -> error "Error: This driver is not supported"
  

loadServerConf configFile = do
  
  conf' <- (\text -> readMaybe text :: Maybe ServerConfig) <$> readFile configFile
    

  case conf' of

    Just conf -> do
      let database = serverDatabase conf
      let port     = serverPort conf
      let path     = serverStoragePath conf
      let sconf    = Conf port Nothing Nothing 30 Nothing

      let sqlitePath = (stripPrefix "sqlite://" database)

      putStrLn database
      putStrLn sqlitePath
      
      case parseDbDriver database of
        "sqlite" ->    
          withConnServer Sqlite3.connectSqlite3
                          sqlitePath 
                          sconf
                          routes
        "postgres" ->
          withConnServer Pg.connectPostgreSQL database sconf routes

        _         -> error "Error: Database not supported"

    Nothing   -> putStrLn "Error: failed parse the config file"
  

 -- "sqlite:///home/archmaster/projects/zhserver/db/zotero.sqlite"

main = do
  
  putStrLn "Server Running"

  loadServerConf "zhserver.conf"

  -- withConnServer2 Pg.connectPostgreSQL
  --                "postgres://postgres@localhost/zotero"
  --                serverConf
  --                routes

  -- withConn Pg.connectPostgreSQL
  --          "postgres://postgres@localhost/zotero"
           
  

 {- --------------- ROUTES ----------------}


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
