{-# LANGUAGE DeriveDataTypeable #-}

module Zotero
       (
         withConnection
         ,getCollections
         ,showCollections
         ,collectionItems
         ,itemTagsData
         ,itemTags
         ,itemAttachmentData
         ,itemAttachmentFile
         ,itemAuthors
         ,sqlQuery
         ,sqlQueryAll
         ,sqlQueryColumn
         ,main          
         ,database
         ,storagePath
         ,dbConnection

         ,getCollectionItemsJSON
         ,getZoteroItemJSON
         ,getZoteroItemsJSON

       ) where


import qualified Database.HDBC.Sqlite3 as SQLite
import qualified Database.HDBC as HDBC


import Data.Maybe (catMaybes, maybe, fromJust, fromMaybe)
import Data.List (lookup)
import Control.Monad

import qualified System.FilePath as SF

import qualified Data.Text as T 
import qualified System.Process as P

import Text.Read (readMaybe)
import Text.Printf (printf)

import Data.Int -- Int64 

import System.Exit  (exitSuccess)

import System.Directory (copyFile, createDirectoryIfMissing)

import Text.JSON.Generic 




type SQLQuery =  [(String, HDBC.SqlValue)] 



data ZoteroItem =
  ZoteroItem {    zoteroItemID          :: Int
                , zoteroItemData        :: [(String, String)]                                           
                , zoteroItemTags        :: [(Int, String)] -- (tagID, tag)
                , zoteroItemCollections :: [(Int, String)] -- (collID, collection)
             
                , zoteroItemFile        :: Maybe String    -- File attachment
                , zoteroItemMime        :: Maybe String    -- Mime Type                                                                           
             } deriving (Eq, Show, Read, Data, Typeable)
            

 

database =  "/home/archmaster/zotero.sqlite"

storagePath = "/home/archmaster/.mozilla/firefox/mwad0hks.zotero/zotero/storage"


splitOn delim text =  
   map T.unpack $ T.splitOn (T.pack delim) (T.pack text)

strip  = T.unpack . T.strip . T.pack

fromInt64ToInt :: Int64 -> Int
fromInt64ToInt = fromIntegral

fromIntToInt64 :: Int -> Int64
fromIntToInt64 = fromIntegral


dbConnection =
  SQLite.connectSqlite3 database 

joinStrings :: String -> [String] -> String
joinStrings common strs =
  case strs of
    [] -> ""
    _  -> foldr1 (\x acc ->  x ++ common ++  acc) strs 

lookupString :: String -> SQLQuery -> Maybe String 
lookupString field row = do 
  value <- lookup field row
  return $ HDBC.fromSql value

  
lookupInt :: String -> SQLQuery -> Maybe Int
lookupInt field row = do 
  value <- lookup field row
  return $ HDBC.fromSql value 

coerceString :: [HDBC.SqlValue] -> Int -> Maybe String
coerceString sqlValues pos =
  HDBC.fromSql (sqlValues !! pos)

coerceInt :: [HDBC.SqlValue] -> Int -> Maybe Int
coerceInt sqlValues pos = 
  HDBC.fromSql (sqlValues !! pos)

fromSqlToInt :: HDBC.SqlValue -> Int
fromSqlToInt  sv = HDBC.fromSql sv  

fromSqlToString :: HDBC.SqlValue -> String
fromSqlToString sv = HDBC.fromSql sv 

sqlQuery :: HDBC.IConnection conn =>  conn
     -> String
     -> [HDBC.SqlValue]
     -> ([HDBC.SqlValue] -> b)
     -> IO (Maybe b)
sqlQuery conn sql sqlvals projection = do
  stmt <- HDBC.prepare conn sql
  HDBC.execute stmt sqlvals
  row <- HDBC.fetchRow stmt
  return (fmap projection row)

  
sqlQueryAll
  :: HDBC.IConnection conn =>
     conn
     -> String -> [HDBC.SqlValue] -> ([HDBC.SqlValue] -> b) -> IO [b]    
sqlQueryAll conn sql sqlvals projection = do
  stmt <- HDBC.prepare conn sql
  HDBC.execute stmt sqlvals  
  rows <- HDBC.fetchAllRows stmt
  return (map projection rows)

sqlQueryColumn :: HDBC.IConnection conn => conn
               -> String
               -> [HDBC.SqlValue] -> (HDBC.SqlValue -> b) -> IO [b]     
sqlQueryColumn conn sql sqlvals coercion = do
  result <- sqlQueryAll conn sql sqlvals (coercion . (!!0))
  return $ result
    
        
withConnection :: HDBC.IConnection  conn => IO conn -> (conn -> IO r) -> IO r
withConnection ioConn function = do
  conn   <- ioConn
  result <- function conn
  return result

getCollections :: HDBC.IConnection conn => conn -> IO [(Int, String)]
getCollections conn = do
  
  rows <- sqlQueryAll conn sql [] projection
  return rows                   
  
    where
      
      sql = unlines   ["SELECT collectionID, collectionName",
                       "FROM collections"
                      ]
        
      projection  =  fromJust . (\row -> (,)
                                <$> coerceInt    row 0
                                <*> coerceString row 1)

  
showCollections conn =
  mapM_ print =<< getCollections conn 


collectionItems :: HDBC.IConnection conn => conn -> Int -> IO [Int]
collectionItems conn collID =
  let collID' = fromIntToInt64 collID in 

  sqlQueryColumn conn sql [HDBC.SqlInt64 collID'] fromSqlToInt
  
  where
    sql = "SELECT  itemID FROM collectionItems WHERE collectionID = ?"


itemTagsData :: HDBC.IConnection conn => conn -> Int -> IO [(Int, String)]
itemTagsData conn itemID = do
  
  let itemID' = fromIntToInt64 itemID in

    sqlQueryAll conn sql [HDBC.SqlInt64 itemID'] projection

  where
    
    sql = unlines ["SELECT  tags.tagID, tags.name",
                   "FROM    itemTags, tags", 
                   "WHERE   itemTags.tagID = tags.tagID", 
                   "AND     itemID = ?"
                   ]

    projection xs = (fromSqlToInt (xs !! 0), fromSqlToString (xs !! 1))


itemTags conn itemID =
  map snd <$> itemTagsData conn itemID 

itemAttachmentData :: HDBC.IConnection conn => conn -> Int -> IO (Maybe [String])
itemAttachmentData conn itemID =
  
  let itemID' = fromIntToInt64 itemID in

  sqlQuery conn sql [HDBC.SqlInt64 itemID', HDBC.SqlInt64 itemID'] projection

  where 

    sql = unlines $

      [
        "SELECT  itemAttachments.path, items.key, itemAttachments.mimeType, itemTypes.typeName",
        "FROM    items, itemAttachments, itemTypes",
        "WHERE   itemAttachments.itemID = items.itemID",
        "AND     itemTypes.itemTypeID = items.itemTypeID",
        "AND     (itemAttachments.sourceItemID = ? OR items.itemID = ?)"
--        "AND     itemAttachments.sourceItemID = ?"
        ]

    projection = map fromSqlToString


itemAttachmentFile :: HDBC.IConnection conn => conn -> FilePath -> Int -> IO (Maybe FilePath)
itemAttachmentFile conn storagePath itemID = do
  
  attachmentData <-itemAttachmentData conn itemID

  return $ attachmetFile attachmentData

  where
    
      -- Maybe Monad 
    attachmetFile attachmentData = do
      
      attachdata <- attachmentData

      let path = splitOn ":" (attachdata !! 0) !! 1
      let key  = attachdata !! 1 
          
      return $ SF.joinPath [storagePath, key, path]

  

--itemData :: HDBC.IConnection conn => conn -> Int -> IO [(String, String)
itemData conn itemID =
  
  let itemID' = fromIntToInt64 itemID in

  sqlQueryAll conn sql [HDBC.SqlInt64 itemID'] projection

  where
     
    sql = unlines $
      ["SELECT  fields.fieldName, itemDataValues.value",
       "FROM    fields, itemDataValues, itemData",
       "WHERE   itemData.fieldID = fields.fieldID",
       "AND     itemData.valueID = itemDataValues.valueID",
       "AND     itemData.itemID = ?"
      ]     

    projection = \row ->
      (fromSqlToString $row !! 0, fromSqlToString $ row !! 1)

{-  Query authors given the itemID.-}
itemAuthors conn itemID =

  let itemID' = fromIntToInt64 itemID in

  sqlQueryAll conn sql [HDBC.SqlInt64 itemID'] (map fromSqlToString)

  where

    sql = unlines $ [
      
      "SELECT   creatorData.firstName, creatorData.lastName, creatorTypes.creatorType",
      "FROM     creatorData, creatorTypes, creators, itemCreators",
      "WHERE    itemCreators.creatorID = creators.creatorID",
      "AND      itemCreators.creatorTypeID = creatorTypes.creatorTypeID",
      "AND      creators.creatorDataID = creatorData.creatorDataID",
      "AND      itemCreators.itemID = ?"
      ]



getZoteroItem conn itemID = do
  
  itemData    <- itemData conn itemID
  itemTags    <- return []
  itemColls   <- return []
  
  itemFile    <- itemAttachmentFile conn storagePath  itemID
  itemMime    <- return Nothing

  return $ ZoteroItem itemID
                      itemData 
                      itemTags 
                      itemColls 
                      itemFile
                      itemMime

  
getZoteroItemJSON conn itemID =
  encodeJSON <$> getZoteroItem conn itemID 

getZoteroItemsJSON conn itemIDs =
  encodeJSON <$> mapM (getZoteroItem conn) itemIDs 


getCollectionItemsJSON conn collID =
  collectionItems conn collID >>= getZoteroItemsJSON conn

{- Last name comes first than first name in 
   order to make easier to locate the author.
-}   
printItemAuthor conn itemID = do

  authorData <- itemAuthors conn itemID
  mapM_ printRow authorData

  where
    printRow row = printf "%s: %s, %s\n" (row !! 2) (row !! 1) (row !! 0)


ignore :: IO a -> IO ()
ignore ioValue = do
  a <- ioValue
  return ()

printItem conn itemID = do
  
  itemdata <- itemData conn itemID

  path <- itemAttachmentFile conn storagePath itemID

  tags <- itemTags conn itemID
  

  let  printField label field = do        
         
         mapM_ (\value -> do
                           putStr label
                           putStr value
                           putStr "\n"                          
               )
           (lookup field itemdata)
         

  putStrLn   ("Item ID: " ++ show itemID)
  putStrLn ""
  printField "Title: "      "title"
  printField "Publisher: "  "publisher"
  printField "Book Title: " "bookTitle"
  printField "Url: "        "url"
  printField "DOI: "        "DOI"

  printItemAuthor conn itemID 
  
  printField "Abstract :\n" "abstractNote"
  putStrLn ""
  mapM_ putStrLn path

  putStrLn $ "Tags: " ++ joinStrings ", " tags

  putStrLn "--------------------------------------------"

-- :{
-- let mapM2 :: (a -> IO b) -> [a] -> IO [b]
--     mapM2 fn xs = sequence $ map fn xs 
-- :}

printCollection conn collID = do
  items <- collectionItems conn collID
  mapM_ (printItem conn) items 
  
{-
Copy a zotero collection to a given directory given the
collection ID and the destiny directory.

 - conn   -> Database Connection
 - collID -> Collection ID
 - dest   -> Destiny directory 

-}
copyCollectionTo conn collID dest = do
  
  items       <- collectionItems conn collID

  attachments <- mapM (itemAttachmentFile conn storagePath) items
  
  createDirectoryIfMissing True dest

  mapM_ copyToDest attachments

  where

    destPath itemFile =
      SF.joinPath [dest, SF.takeFileName itemFile]

    copyToDest :: Maybe FilePath -> IO ()
    copyToDest itemFile =
      mapM_ (\i -> copyFile i (destPath i)) itemFile  


-- let destPath itemFile = SF.joinPath ["/tmp/test", SF.takeFileName itemFile]    

prompt msg = do
  putStr  msg
  line    <- getLine
  return line 





--main :: IO ()
main = forever $ do

  conn <- dbConnection

  putStrLn "\n"
  putStrLn "Enter 1 to list the collections "
  putStrLn "Enter 2 to list a collection given collectionID"
  putStrLn "Enter 3 to open a itemID"
  putStrLn "Enter 4 Copy the a collection to a given directory"
  putStrLn "Enter 5 to exit the library"
  putStrLn "\n"
  putStrLn "--------------------"

  choice <- strip <$> getLine

  case choice of
    
    "1" -> showCollections conn 
    
    "2" -> do
             input <- prompt "Enter a collection ID: "  
             mapM_ (printCollection conn) (readMaybe input :: Maybe Int)

    "3" -> do
             input <- prompt "Enter a item ID: "

             let itemID = readMaybe input :: Maybe Int 

             mapM_ (\fid -> printItem conn fid ) itemID

             path <- fmap join $ mapM
               (\itemID -> itemAttachmentFile conn storagePath itemID)
               itemID

             mapM_ (\p -> P.system $ "xdg-open " ++ "\"" ++ p ++ "\"") path


    "4"  -> do  collID  <- prompt "Enter a collection ID: "                

                let collID' = readMaybe collID :: Maybe Int

                case collID' of
                  
                  Just collid -> do
                               destDir <- prompt "Enter the destination directory "
                               copyCollectionTo conn collid destDir
                               putStrLn "Collection Copied OK."

                  Nothing -> putStrLn "Failed not a valid collection number"

    "5"  -> exitSuccess
            
             
  
    _  -> putStrLn "Error Enter with another option"
            
            
  

    


     
{-

:load zotero.hs
conn <- dbConnection
coll <- head <$>  getCollections conn

 row <-  sqlQuery conn "SELECT * FROM collections" [] 

-}
