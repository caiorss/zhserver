{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- # LANGUAGE DeriveDataTypeable #-}

module Zotero
       (
         withConnection
         ,getCollections
         ,showCollections
         ,collectionItems
         ,itemTagsData
         ,itemTags
         ,itemData
         ,itemAttachmentData
         ,itemAttachmentFile
         ,itemAuthors
         ,sqlQuery
         ,sqlQueryAll
         ,sqlQueryColumn
         ,database
         ,storagePath
         ,dbConnection

         ,getTagItems
         ,getRelatedTags
         ,getTagsFromCollection
         ,searchByTitleWordLike

          {- JSON Export Functions -}
         ,getTagsJSON          
         ,getZoteroItem
         ,getZoteroItemJSON
         ,getZoteroItemsJSON
         ,getCollectionItemsJSON
         ,getTagItemsJSON
         ,getItemsFromAuthorJSON        
         ,getAuthorsJSON
         ,getRelatedTagsJSON
         ,getTagsFromCollectionJSON
         ,searchByTitleWordLikeJSON
          
         ,getItemsFromAuthor         
         ,getAuthors


         ,joinStrings
         ,strip

       ) where


import qualified Database.HDBC.Sqlite3 as SQLite
import qualified Database.HDBC.PostgreSQL as PgSQL
import qualified Database.HDBC as HDBC


import Data.Maybe (catMaybes, maybe, fromJust, fromMaybe)
import Data.List (lookup)
import Control.Monad

import qualified System.FilePath as SF

import qualified Data.Text as T 


import Data.Int -- Int64 



import System.Directory (copyFile, createDirectoryIfMissing)

-- import Text.JSON.Generic
import Data.Aeson 
import GHC.Generics


type SQLQuery =  [(String, HDBC.SqlValue)] 



data ZoteroItem =
  ZoteroItem {    zoteroItemID          :: Int
                , zoteroItemData        :: [(String, String)]                                           
                , zoteroItemTags        :: [(Int, String)] -- (tagID, tag)
                , zoteroItemCollections :: [(Int, String)] -- (collID, collection)
                                           
                , zoteroItemFile        :: Maybe String    -- File attachment
                , zoteroItemMime        :: Maybe String    -- Mime Type                                                                           
             } deriving (Eq, Show, Read,  Generic)



instance FromJSON ZoteroItem

instance ToJSON ZoteroItem where
  toJSON (ZoteroItem itemID itemData itemTags
          itemColls itemFile itemMime) = object
    [
       "id"    .= itemID
      ,"data"  .= itemData
      ,"tags"  .= itemTags
      ,"colls" .= itemColls
      ,"file"  .= itemFile
      ,"mime"  .= itemMime
    ]
    



data ZoteroAuthor =
  ZoteroAuthor  { zoteroAuthorID          :: Int
                , zoteroAuthorFirstName   :: String 
                , zoteroAuthorLastName    :: String                                                                          
             } deriving (Eq, Show, Read,  Generic)




instance ToJSON ZoteroAuthor where
  toJSON (ZoteroAuthor authorID firstName lastName) = object
    [
       "id"    .= authorID
      ,"first" .= firstName
      ,"last"  .= lastName
    ]


data ZoteroTag =
  ZoteroTag {   zoteroTagID   :: Int
              , zoteroTagName :: String
                                 
            } deriving (Eq, Show, Read, Generic)
  

instance ToJSON ZoteroTag where
  toJSON  (ZoteroTag tagID name) = object
    [   "id"    .= tagID
        ,"name" .= name
    ]


data ZoteroColl =
  ZoteroColl {  zoteroCollID   :: Int
               ,zoteroCollName :: String                                
             } deriving (Eq, Show, Read, Generic)


instance ToJSON ZoteroColl where
  toJSON  (ZoteroColl collID name) = object
    [   "id"    .= collID
        ,"name" .= name
    ]



-- instance ToJSON ZoteroItem 
 

database =  "/home/archmaster/zotero.sqlite"

storagePath = "/home/archmaster/.mozilla/firefox/mwad0hks.zotero/zotero/storage"


-- dbConnection = SQLite.connectSqlite3 database
dbConnection = PgSQL.connectPostgreSQL "postgres://postgres@localhost/zotero"

getZoteroItem conn itemID = do
  
  itemData    <- itemData conn itemID
  itemTags    <- itemTagsData conn itemID 
  itemColls   <- return  [] -- itemCollections conn itemID
  
  itemFile    <- itemAttachmentFile conn itemID
  itemMime    <- return Nothing

  return $ ZoteroItem itemID
                      itemData 
                      itemTags 
                      itemColls 
                      itemFile
                      itemMime


splitOn delim text =  
   map T.unpack $ T.splitOn (T.pack delim) (T.pack text)

strip  = T.unpack . T.strip . T.pack

fromInt64ToInt :: Int64 -> Int
fromInt64ToInt = fromIntegral

fromIntToInt64 :: Int -> Int64
fromIntToInt64 = fromIntegral



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
               -> [HDBC.SqlValue]
               -> (HDBC.SqlValue -> b)
               -> IO [b]     
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




itemCollections conn itemID = do

  let itemID' = fromIntToInt64 itemID in

    sqlQueryAll conn sql [HDBC.SqlInt64 itemID'] projection


    where
      
      projection xs = (fromSqlToInt (xs !! 0), fromSqlToString (xs !! 1))

      sql = "SELECT collectionID FROM   collectionItems WHERE  itemID = ?"



-- Return all tags in the database
-- 
getTags :: HDBC.IConnection conn => conn ->  IO [ZoteroTag]
getTags conn =

  sqlQueryAll conn sql [] projection 
  
  where
    sql = "SELECT tagID, name FROM tags"

    projection row = ZoteroTag (fromSqlToInt (row !! 0))
                               (fromSqlToString (row !! 1))


getTagsJSON conn =
  encode <$> getTags conn 


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


itemAttachmentFile :: HDBC.IConnection conn => conn ->  Int -> IO (Maybe FilePath)
itemAttachmentFile conn itemID = do
  
  attachmentData <-itemAttachmentData conn itemID

  return $ attachmetFile attachmentData

  where
    
      -- Maybe Monad 
    attachmetFile attachmentData = do
      
      attachdata <- attachmentData

      let path = splitOn ":" (attachdata !! 0) !! 1
      let key  = attachdata !! 1 
          
      return $ SF.joinPath [ key, path]

  

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






ignore :: IO a -> IO ()
ignore ioValue = do
  a <- ioValue
  return ()

-- :{
-- let mapM2 :: (a -> IO b) -> [a] -> IO [b]
--     mapM2 fn xs = sequence $ map fn xs 
-- :}

{-
Copy a zotero collection to a given directory given the
collection ID and the destiny directory.

 - conn   -> Database Connection
 - collID -> Collection ID
 - dest   -> Destiny directory 

-}

  
getZoteroItemJSON conn itemID =
  encode <$> getZoteroItem conn itemID 

getZoteroItemsJSON conn itemIDs =
  encode <$> mapM (getZoteroItem conn) itemIDs 


getCollectionItemsJSON conn collID =
  collectionItems conn collID >>= getZoteroItemsJSON conn


getTagItems conn tagID =

  let tagID' = fromIntToInt64 tagID in

  sqlQueryColumn conn sql [HDBC.SqlInt64 tagID'] fromSqlToInt

  where
    sql = unlines $ ["SELECT itemID", 
                     "FROM itemTags", 
                     "WHERE tagID = ?"
                     ]

getTagItemsJSON conn tagID =
  getTagItems conn tagID >>= getZoteroItemsJSON conn


getAuthors conn =

  sqlQueryAll conn sql [] projection 

  where

    sql = "SELECT creatorDataID, firstName, lastName FROM creatorData"
  
    projection row = ZoteroAuthor (fromSqlToInt    (row !! 0))
                                  (fromSqlToString (row !! 1))
                                  (fromSqlToString (row !! 2))


getAuthorsJSON conn  =
  encode <$> getAuthors conn


getItemsFromAuthor conn authorID =

  let authorID' = fromIntToInt64 authorID in
  
  sqlQueryColumn conn sql [HDBC.SqlInt64 authorID'] fromSqlToInt

  where

    sql = "SELECT itemID FROM itemCreators WHERE creatorID = ?"


getItemsFromAuthorJSON conn authorID =
  getItemsFromAuthor conn authorID >>= getZoteroItemsJSON conn 



getRelatedTags conn tagID =

  let tagID' = fromIntToInt64 tagID in

  sqlQueryAll conn sql [HDBC.SqlInt64 tagID', HDBC.SqlInt64 tagID'] projection  

  where

    projection row = ZoteroTag (fromSqlToInt    (row !! 0))
                               (fromSqlToString (row !! 1))

    sql = unlines $  ["SELECT DISTINCT itemTags.tagID, tags.name",
                     "FROM   itemTags, tags             ",
                     "WHERE  itemID IN (SELECT itemID   ", 
                     "                  FROM   itemTags ", 
                     "                  WHERE  tagID =? ",
                     "                  )               ",
                     "AND   itemTags.tagID = tags.tagID ",
                     "AND   tags.tagID != ?             "
                    ]   

getRelatedTagsJSON conn tagID =
  encode <$> getRelatedTags conn tagID 


getTagsFromCollection conn collID =

  let collID' = fromIntToInt64 collID in

  sqlQueryAll conn sql [HDBC.SqlInt64 collID'] projection 
  
  where

    projection row = ZoteroTag (fromSqlToInt    (row !! 0))
                               (fromSqlToString (row !! 1))    
    
    sql = unlines $ [ "SELECT DISTINCT itemTags.tagID, tags.name",
                      "FROM   itemTags, tags",
                      "WHERE  itemTags.itemID IN", 
                      "( SELECT itemID", 
                      "FROM   collectionItems",
                      "WHERE  collectionItems.collectionID = ?",
                      ")",
                      "AND    itemTags.tagID = tags.tagID"
                     ]


getTagsFromCollectionJSON conn collID =
  encode <$> getTagsFromCollection conn collID


searchByTitleWordLike conn searchWord =

  sqlQueryColumn conn sql [HDBC.SqlString searchWord] fromSqlToInt
  
  where

    sql = unlines $ [

       "SELECT itemData.itemID"  
      ,"FROM   itemData, itemDataValues, itemAttachments"
      ,"WHERE  fieldID = 110" 
      ,"AND    itemData.valueID = itemDataValues.valueID"
      ,"AND    itemAttachments.sourceItemID = itemData.itemID"
      ,"AND    itemDataValues.value LIKE ?"      
      ]

searchByTitleWordLikeJSON conn searchWord =
  searchByTitleWordLike conn searchWord >>= getZoteroItemsJSON conn 

    

{-

:load zotero.hs
conn <- dbConnection
coll <- head <$>  getCollections conn

 row <-  sqlQuery conn "SELECT * FROM collections" [] 

-}
