

{- LANGUAGE FlexibleContexts, OverlappingInstances, RankNTypes, RecordWildCards #-}


import qualified Zotero as Z

import Control.Monad.Trans (liftIO, lift)

import Data.Text                  (Text)

import Control.Monad    (msum, mapM, mapM_, foldM)

import Text.Read (readMaybe)

import Happstack.Server (FromReqURI (..), dir, Conf, Conf (..), nullConf, ok
                         , seeOther, simpleHTTP, dir, dirs, path, seeOther, method
                         , Method (GET, POST, HEAD)
                         , ServerPart, ServerPartT, look
                         , flatten, toResponse 
                        
                        )

import Happstack.Server ( Browsing(EnableBrowsing, DisableBrowsing),
                          nullConf
                        , serveDirectory, simpleHTTP
                        )

import Happstack.Server.FileServe ( asContentType
                                   ,mimeTypes
                                   ,serveFile
                                  )       

import qualified Data.ByteString.Lazy as BL 
import qualified Data.ByteString.Lazy.Char8 as LC 



import Text.Printf (printf)

-- Quote a string 
strQuote s = "\"" ++ s ++ "\""

joinStrBy :: String -> [String] -> String 
joinStrBy bystr strlist =
  case strlist of
    [] -> ""
    _  -> foldr1 (\x acc -> x ++  bystr ++ acc  )  strlist 


joinStrByFn :: String -> (a -> String) -> [a] -> String 
joinStrByFn bystr fn xs =
  case xs of
    [] -> ""
    _  -> foldr (\x acc -> fn x ++  bystr ++ acc ) acc0 xss
            where
              acc0 = fn $ head xs
              xss  = tail xs
              


{-
   >>> tupleToJSON show show "key" "val" (10, "text")
   "{key: 10, val: \"text\"}"
   >>> 
-}
tupleToJSON :: (key -> String) -- Function to format the key 
            -> (val -> String) -- Function to format the value 
            -> String          -- Key Label               
            -> String          -- Value Label
            -> (key, val)      -- Tuple             
            -> String
            
tupleToJSON fnKey fnVal keyLabel valLabel tuple  =
  printf "{%s: %s, %s: %s}" (strQuote keyLabel)
                            (fnKey . fst $ tuple)
                            (strQuote valLabel)
                            (fnVal . snd $ tuple) 


tupleColumnToJSON :: (Show key, Show val) =>
        String
     -> String
     -> [(key, val)]
     -> String 
tupleColumnToJSON keyLabel valLabel tupleList =
  printf "[%s]" $ joinStrByFn ",\n" fn tupleList
    where
      fn = tupleToJSON show show keyLabel valLabel
  

withConn = Z.withConnection Z.dbConnection


collectionsToJSON = do
  colls <- withConn Z.getCollections
  return $ tupleColumnToJSON "id" "coll" colls 

-- serverConf :: Conf 
-- serverConf = Conf
--   {
--       port       = 8080
--     , validator  = Nothing
--     , logAccess  = Nothing 
--     , timeout    = 30

--   }


-- runServer :: IO ()
runServer =  simpleHTTP nullConf $
  msum [
         
            flatten $ dir "api" $ dir "colls" $ routeCollectionID

          , flatten $ dir "api" $ dir "colls" $ routeTagsFromCollID
           
          , flatten $ dir "api" $ dir "colls"  $ routeCollection

          , flatten $ dir "api" $ dir "tags" $ routeTagID
            
          , flatten $ dir "api" $ dir "tags" $ routeTags

          , flatten $ dir "api" $ dir "reltags" $ routeRealtedTags
       

          , flatten $ dir "api" $ dir "authors" $ routeAuthorID
            
          , flatten $ dir "api" $ dir "authors" $ routeAuthors 

          , flatten $ dir "static" $  serveDirectory EnableBrowsing
                                                  ["index.html",
                                                   "style.css",
                                                   "loader.js"
                                                  ]
                                                  "."
                                                  
          , flatten $ dir "attachment" $ serveDirectory DisableBrowsing [] Z.storagePath                                      
          
          , flatten $ dir "wiki" $ serveDirectory EnableBrowsing ["Index.wiki.html"] "/home/archmaster/org/wiki"

          
          , flatten $ seeOther "static" "static"          
          ]


routeCollection :: ServerPartT IO String
routeCollection =  do
  colls <- liftIO collectionsToJSON    
  return colls
                                 

routeCollectionID :: ServerPartT IO String
routeCollectionID = do

  queryCollID <- look "id"

  let collID = readMaybe queryCollID :: Maybe Int 
    
  liftIO $ putStrLn ("Received Request : " ++ queryCollID)

  case collID of
    
    Just collID' -> do
      json <-  liftIO $ withConn (\c -> Z.getCollectionItemsJSON c collID')
      return $ LC.unpack json

    Nothing -> return ""          


routeTags :: ServerPartT IO String
routeTags = do
  json <- liftIO $ withConn Z.getTagsJSON
  return $ LC.unpack json




routeTagID :: ServerPartT IO String
routeTagID = do

  queryTagID <- look "id"

  let tagID = readMaybe queryTagID :: Maybe Int 

  case tagID of

    Just tagID' -> do 
                      json <- liftIO $ withConn (\c -> Z.getTagItemsJSON c tagID')
                      return $ LC.unpack json

    Nothing -> return ""



routeRealtedTags :: ServerPartT IO String
routeRealtedTags = do

  queryTagID <- look "id"

  let tagID = readMaybe queryTagID :: Maybe Int 

  case tagID of

    Just tagID' -> do 
                      json <- liftIO $ withConn (\c -> Z.getRelatedTagsJSON c tagID')
                      return $ LC.unpack json

    Nothing -> return ""




routeTagsFromCollID :: ServerPartT IO String
routeTagsFromCollID = do

  queryTagID <- look "col2tag"

  let tagID = readMaybe queryTagID :: Maybe Int 

  case tagID of

    Just tagID' -> do 
          json <- liftIO $ withConn (\c -> Z.getTagsFromCollectionJSON c tagID')
          return $ LC.unpack json

    Nothing -> return ""



routeAuthorID :: ServerPartT IO String
routeAuthorID = do

  queryAuthorID <- look "id"

  let authorID = readMaybe queryAuthorID :: Maybe Int 

  case authorID of

    Just authorID' -> do 
                      json <- liftIO $ withConn (\c -> Z.getItemsFromAuthorJSON c authorID')
                        
                      return $ LC.unpack json

    Nothing -> return ""



routeAuthors :: ServerPartT IO String
routeAuthors = do
  json <- liftIO $ withConn Z.getAuthorsJSON
  return $ LC.unpack json

        
main :: IO ()
main = do
  putStrLn "Starting Server ..."
  runServer 


       


