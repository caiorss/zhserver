{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}


{- |
Module      : Main
Description : Zotero Web Server and REST  API
License     : Public Domain

Zotero Web Server and Web API.

-}
module Main where 

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

import Happstack.Server.Internal.Types

-- import Happstack.Server (FromReqURI (..), dir, Conf, Conf (..), nullConf, ok
--                          , seeOther, simpleHTTP, dir, dirs, path, seeOther, method
--                          , Method (GET, POST, HEAD)
--                          , ServerPart, ServerPartT, look
--                          , flatten, toResponse, askRq, basicAuth
                        
--                         )

    
import qualified Happstack.Server as HS
import qualified Happstack.Server.Auth as HSA
    

import Happstack.Server (ServerPart, ServerPartT, flatten, dir, look)

import Happstack.Server.FileServe ( asContentType
                                   ,mimeTypes
                                   ,serveFile
                                  )       

import qualified Data.ByteString.Lazy as BL 
import qualified Data.ByteString.Lazy.Char8 as LC 

import qualified Database.HDBC as HDBC
import qualified Database.HDBC.PostgreSQL as Pg
import qualified Database.HDBC.Sqlite3    as Sqlite3 

import qualified Data.Map as M
    
import Text.Printf (printf)

import qualified Zotero as Z
import qualified DBUtils
    
import Zotero (DBConn)

import Text.Show.Pretty (pPrint)

{-
   ReaderT conn (ServerPartT IO) response
    = conn -> (ServerPartT IO) response
    = conn -> ServerPartT IO response

-}
type ServerApp a =  forall conn. (HDBC.IConnection conn)
                    => ReaderT conn (ServerPartT IO) a

{- | Server Authentication -}
data Auth = AuthEmpty                {- | No Authentication    -}
          | AuthBasic String String  {- | Basic authentication -}
          | AuthHtml  String String  {- | Html form Authentication -}
            deriving (Eq, Read, Show)

data ServerConfig = ServerConfig
                    {
                     serverPort        :: Int      -- Port that server will listen
                    ,serverHost        :: String   -- Host that server will listen
                    ,serverLogin       :: Auth     -- Server Master login / password
                    ,serverStaticPath  :: String   -- Single Page App static files like /index.html, /js/script.js
                    ,serverStoragePath :: String   -- Zotero storage Path
                    ,serverDatabase    :: String   -- Server Database URI
                    } deriving (Eq, Show, Read)



parseInt :: String -> Maybe Int
parseInt s = readMaybe s



{- Function which creates a server configuration -}
makeServerConf :: Int -> Conf
makeServerConf port = Conf
  { port      = port
  , validator = Nothing
  , logAccess = Nothing
  , timeout   = 30
  , threadGroup = Nothing
  }



basicAuth :: String -> String ->  ServerApp Response ->  ServerApp Response
basicAuth login passwd server =
    HSA.basicAuth "127.0.0.1" (M.fromList [(login, passwd)]) server


makeHttpLogger :: ServerApp ServerTypes.Response -> ServerApp ServerTypes.Response
makeHttpLogger server = do
  rq <- HS.askRq
  puts $ "=============== REQUEST ================"
  puts $ "Method = " ++ (show $ rqMethod rq)
  puts $ "Paths  = " ++ (show $ rqPaths rq)
  puts $ "Uri    = " ++ (show $ rqUri rq)
  puts $ "Query  = " ++ (show $ rqQuery rq)
  puts $ "Peer   = " ++ (show $ rqPeer rq)
  puts "\n\nHeaders -------"
  liftIO $ pPrint $ rqHeaders rq

  -- Uncomment the line below to print the full request log.
  --
  -- puts "------------------\n Full Request Log ---"
  -- pPrint rq
  server
  where
    puts s = liftIO $ putStrLn s


withConnServerDB ::  Response.ToMessage a =>
                     String         -- Database URI
                  -> Conf           -- Server Configuration
                  -> ServerApp a    -- Server Monad
                  -> IO ()
withConnServerDB dbUri conf serverApp = do
  maybeConn <- Z.openDBConnection dbUri

  case maybeConn of

    Just (DBUtils.HDBConnSqlite conn)   ->  withConn conn conf serverApp
    Just (DBUtils.HDBConnPostgres conn) ->  withConn conn conf serverApp
    Nothing                     ->  putStrLn $ "Error: Invalid database URI " ++ dbUri

  where
    withConn conn conf serverApp = do
      -- Listen http request
      HS.simpleHTTP conf $ runReaderT serverApp conn
      -- Disconnect database
      -- HDBC.disconnect conn
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
  HS.simpleHTTP conf $ runReaderT serverApp conn
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
  HS.simpleHTTP conf $ runReaderT serverWrapper conn1
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
  param      <- HS.look paramName
  let param' = parser param
  maybe (return err) (\p -> runDbQuery $ dbFn p) param'

{-| Creates a server route for which the paramter is
an ID (int) - Identification Number for the database query 
-}
serverRouteParamID :: String
              -> (Int -> DBConn LC.ByteString)
              -> ServerApp LC.ByteString
serverRouteParamID param dbFn = 
  serverRouteParam parseInt param LC.empty dbFn


{-| Creates a server route for which the paramter is 
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
    flatten $ dir "api" $ dir "item" $  serverRouteParamID "id" Z.getZoteroItemIdAsListJSON


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


    -- Returns all items from a given tag
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

  -- Return all authors
  -- API End point  /api/authors 
  -- 
  , flatten $ dir "api" $ dir "authors" $ routeAuthors

  
  , flatten $ dir "api" $ dir "relatedtags" $ routeRealatedTags                 

  -- Search items which title contains a word
  -- API End Point - /api/search?title=<search word>
  , flatten $ dir "api" $ dir "search" $ routeSearchByTitleLike

  -- Search items which content contains word   
  , flatten $ dir "api" $ dir "search" $ routeSearchByContentAndTitleLike 
   
    
    -- Zotero Attachment Files - Serve attachment files in storagePath directory 
    -- API End Point /api/search?content=<search word>
    -- 
  , flatten $ dir "attachment" $ HS.serveDirectory HS.DisableBrowsing [] storagePath


  -- Single Page App static files
  , flatten $ HS.serveDirectory HS.DisableBrowsing [  "index.html"
                                                     ,"style.css"
                                                     ,"loader.js"
                                                     ,"*.html"
                                                     ,"*.js"
                                                   ]
                                                  staticPath
                                                  
 -- , flatten $ seeOther "static" "/"
  , flatten $ Response.notFound $ "Error: Not found"                                              
    
  ]

{- | Start server with a given configuration -} 
runServerConf :: ServerConfig -> IO ()
runServerConf conf = do
  let dbUri       = serverDatabase conf
  let port        = serverPort conf
  let storagePath = serverStoragePath conf
  let staticPath  = serverStaticPath conf
  let login       = serverLogin conf                     
  let sconf       = makeServerConf port

  case login of
    AuthEmpty                -> withConnServerDB dbUri sconf $ makeHttpLogger $  makeRoutes staticPath storagePath
                                
    AuthBasic login passwd   -> withConnServerDB dbUri sconf $ (basicAuth login passwd
                                                             $ makeHttpLogger
                                                             $ makeRoutes staticPath storagePath)
    _                        -> error "Error: Not implemented"


runServer host port dbUri staticPath storagePath =
  case readMaybe port :: Maybe Int of
    Just p    -> do  let sconf = makeServerConf p
                     withConnServerDB dbUri sconf (makeHttpLogger $ makeRoutes staticPath storagePath)
    Nothing   -> putStrLn "Error: Invalid port number"

  
-- Start server loading its settings from a configuration file.
--
loadServerConf configFile = do
  conf' <- (\text -> readMaybe text :: Maybe ServerConfig) <$> readFile configFile
  case conf' of
    Just conf -> runServerConf conf
    Nothing   -> putStrLn "Error: failed parse the config file"



{- ================ HTTP ROUTES ======================== -}

{-| Route that displays all collections
    Rest API:   http://localhost:8000/api/colls
-}
routeCollection :: ServerApp LC.ByteString
routeCollection = runDbQuery Z.getCollectionsJSON

{- | Show all items from a given collection defined by its ID
     Rest API -  /api/colls?id=23423 or /api/coll?id={collection ID}
-}
routeCollectionID :: ServerApp LC.ByteString 
routeCollectionID = serverRouteParamID "id" Z.getCollectionItemsJSON

{- |  Returns all items from a given tagID
Rest API - http://localhost:8000/api/tags?id=15  or http://localhost:8000/api/tags?id=[tagID]
-}
routeTagID :: ServerApp LC.ByteString
routeTagID = serverRouteParamID "id" Z.getTagItemsJSON

{- |  Returns all tags 
Rest API - http://localhost:8000/api/tags 
-}  
routeTags :: ServerApp LC.ByteString
routeTags = runDbQuery  Z.getTagsJSON

{- | Get all items belonging to an author, given its ID
Rest API - http://localhost:8000/api/authors?id=100 
-}
routeAuthorID :: ServerApp LC.ByteString
routeAuthorID = serverRouteParamID "id"  Z.getItemsFromAuthorJSON

{- | Return all authors data
Rest API - http://localhost:8000/api/authors
-}  
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

{- | Server http request with all items without collections. -}
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
    


{- ==================== MAIN  ======================== -}

{- | Show user help command -}
showUserHelp = do
  putStrLn "Zhserver -- Your cloud book shelve web server"
  putStrLn ""
  putStrLn "Commands"
  putStrLn ""
  putStrLn "  --env                 - Load configuration file from ZHSERVER_CONFIG environment variable"
  putStrLn "  --conf <config file>  - Load configuration file from <config file>"
  putStrLn ""
  putStrLn "Start server with all configuration passed through command line"
  putStrLn ""
  putStrLn "  --params [host] [port] [dbUri] [staticPath] [storagePath]"
  putStrLn ""
  putStrLn ""
  putStrLn "           - [host]        - Hostname like 0.0.0.0 to listen all hosts "
  putStrLn "           - [port]        - Port like 8080"
  putStrLn "           - [dbUri]       - Database URI"
  putStrLn "           - [staticPath]  - Path to server static files like index.html *.js files"
  putStrLn "           - [storagePath] - Path to Zotero storage directory"


{- | Handle command line arguments -}
parseArgs :: [String] -> IO ()
parseArgs args =
  case args of

  []                -> showUserHelp

  -- Load server configuration file from environment variable
  ["--env"]         ->  do
                           putStrLn "Loading default configuration file from ZHSERVER_CONFIG environment variable."
                           conf  <- lookupEnv "ZHSERVER_CONFIG"
                           case conf of
                             Just file -> loadServerConf file
                             Nothing   -> do putStrLn "Error: Configuration file not found"

  -- Load server configuration from a configuration fiel
  ["--conf", file]   -> loadServerConf file

  -- Load all sever configuration from command line
  ["--params", host, port, dbUri, staticPath, storagePath] -> runServer host port dbUri staticPath storagePath
  
  _                  -> putStrLn "Error: Invalid option."


{- | Main IO Action -}
main :: IO () 
main = do
  getArgs >>= parseArgs
  
