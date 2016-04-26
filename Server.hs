

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
         
           
         --  flatten $ dir "hello" $ path $ \s -> ok $ "Hello, " ++ s
                                      
         -- flatten $ dir "test"         helloPart

           flatten $ dir "api" $ dir "colls" $ routeCollectionID
           
          , flatten $ dir "api" $ dir "colls"  $ routeCollection

         -- , dir "index" $ serveFile (asContentType mimeTypes) "index.html"

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

        
main :: IO ()
main = do
  putStrLn "Starting Server ..."
  runServer 

helloPart :: ServerPart String
helloPart =
    do greeting <- look "greeting"
       noun     <- look "noun"
       ok $ greeting ++ ", " ++ noun

-- makeRoute :: 
-- makeRoute route httpMethod httpResp = do
--   dir route $ method httpMethod $ ok httpResp
  

-- main = simpleHTTP serverConf $ serveDirectory EnableBrowsing [] "."
-- main = simpleHTTP nullConf $ serveDirectory EnableBrowsing [] "."



       


