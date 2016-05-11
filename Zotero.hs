{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

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


import Control.Monad.Trans
import Control.Monad.Trans.Reader

import Data.Maybe (catMaybes, maybe, fromJust, fromMaybe)
import Data.List (lookup)
import Control.Monad

import qualified Database.HDBC.Sqlite3 as SQLite
import qualified Database.HDBC.PostgreSQL as PgSQL
import qualified Database.HDBC as HDBC



import qualified System.FilePath as SF
import qualified Data.Text as T 
import Data.Int -- Int64 
import System.Directory (copyFile, createDirectoryIfMissing)

-- import Text.JSON.Generic
import Data.Aeson 
import GHC.Generics

-- import Data.ByteString (ByteString)
-- import Data.ByteString.Lazy.Internal.ByteString (ByteString)

-- import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Internal as BLI

{- ---------------------- Types -----------------}

type SQLQuery =  [(String, HDBC.SqlValue)] 

type DBConn a = forall conn. (HDBC.IConnection conn) =>  ReaderT conn IO a


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


getZoteroItem :: Int -> DBConn ZoteroItem 
getZoteroItem itemID = do
  
  itemData    <- itemData itemID
  itemTags    <- itemTagsData itemID 
  itemColls   <- return  [] -- itemCollections conn itemID  
  itemFile    <- itemAttachmentFile itemID
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

-- sqlQuery :: HDBC.IConnection conn =>  conn
--      -> String
--      -> [HDBC.SqlValue]
--      -> ([HDBC.SqlValue] -> b)
--      -> IO (Maybe b)

sqlQuery :: String -> [HDBC.SqlValue] -> ([HDBC.SqlValue] -> b) -> DBConn (Maybe b)
sqlQuery sql sqlvals projection = do
  conn   <- ask 
  stmt   <- liftIO $  HDBC.prepare conn sql
  liftIO $ HDBC.execute stmt sqlvals
  row     <- liftIO $ HDBC.fetchRow stmt
  return (fmap projection row)

  
-- sqlQueryAll
--   :: HDBC.IConnection conn =>
--      conn
--      -> String -> [HDBC.SqlValue] -> ([HDBC.SqlValue] -> b) -> IO [b]

sqlQueryAll :: String -> [HDBC.SqlValue] -> ([HDBC.SqlValue] -> b) -> DBConn [b]     
sqlQueryAll sql sqlvals projection = do
  con     <- ask 
  stmt    <- liftIO $ HDBC.prepare con sql
  liftIO  $ HDBC.execute stmt sqlvals  
  rows    <- liftIO $ HDBC.fetchAllRows stmt
  return (map projection rows)


{-
sqlQueryColumn :: HDBC.IConnection conn => conn
               -> String
               -> [HDBC.SqlValue]
               -> (HDBC.SqlValue -> b)
               -> IO [b]
-}

sqlQueryColumn :: String -> [HDBC.SqlValue] -> (HDBC.SqlValue -> b) -> DBConn [b]   
sqlQueryColumn sql sqlvals coercion = do
  sqlQueryAll sql sqlvals (coercion . (!!0))
  
    
        
withConnection :: HDBC.IConnection  conn => IO conn -> (conn -> IO r) -> IO r
withConnection ioConn function = do
  conn   <- ioConn
  result <- function conn
  return result



-- getCollections :: HDBC.IConnection conn => conn -> IO [(Int, String)]

getCollections :: DBConn [(Int, String)]
getCollections = do

  sqlQueryAll sql [] projection
  
    where
      
      sql = unlines   ["SELECT collectionID, collectionName",
                       "FROM collections"
                      ]
        
      projection  =  fromJust . (\row -> (,)
                                <$> coerceInt    row 0
                                <*> coerceString row 1)

--showCollections :: DBConn ()  
showCollections conn = do
  mapM_ print =<< runReaderT getCollections conn 

showCollections2 :: DBConn ()
showCollections2 = do
  colls <- getCollections
  liftIO  (mapM_ print colls)


--collectionItems :: HDBC.IConnection conn => conn -> Int -> IO [Int]
collectionItems :: Int -> DBConn [Int]
collectionItems collID = do
  
  let collID' = fromIntToInt64 collID
  sqlQueryColumn sql [HDBC.SqlInt64 collID'] fromSqlToInt
  
  where
    sql = "SELECT  itemID FROM collectionItems WHERE collectionID = ?"


itemTagsData :: Int -> DBConn [(Int, String)]
itemTagsData itemID = do
  
  let itemID' = fromIntToInt64 itemID 
  sqlQueryAll sql [HDBC.SqlInt64 itemID'] projection

  where
    
    sql = unlines ["SELECT  tags.tagID, tags.name",
                   "FROM    itemTags, tags", 
                   "WHERE   itemTags.tagID = tags.tagID", 
                   "AND     itemID = ?"
                   ]

    projection xs = (fromSqlToInt (xs !! 0), fromSqlToString (xs !! 1))



itemTags :: Int -> DBConn [String]
itemTags itemID =
  map snd <$> itemTagsData itemID 


itemCollections :: Int -> DBConn [(Int, String)]
itemCollections  itemID = do

    let itemID' = fromIntToInt64 itemID    
    sqlQueryAll sql [HDBC.SqlInt64 itemID'] projection

    where
      
      projection xs = (fromSqlToInt (xs !! 0), fromSqlToString (xs !! 1))

      sql = "SELECT collectionID FROM   collectionItems WHERE  itemID = ?"



-- Return all tags in the database
-- 
getTags :: DBConn [ZoteroTag]
getTags = do

  sqlQueryAll sql [] projection 
  
  where
    
    sql = "SELECT tagID, name FROM tags"

    projection row = ZoteroTag (fromSqlToInt (row !! 0))
                               (fromSqlToString (row !! 1))

getTagsJSON :: DBConn BLI.ByteString
getTagsJSON  =  encode <$> getTags


itemAttachmentData :: Int -> DBConn (Maybe [String])
itemAttachmentData itemID = do 
  
  let itemID' = fromIntToInt64 itemID

  sqlQuery sql [HDBC.SqlInt64 itemID', HDBC.SqlInt64 itemID'] projection

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


itemAttachmentFile :: Int -> DBConn (Maybe FilePath)
itemAttachmentFile itemID = do
  
  attachmentData <- itemAttachmentData itemID
  return $  attachmetFile attachmentData

  where
    
      -- Maybe Monad 
    attachmetFile attachmentData = do
      
      attachdata <- attachmentData

      let path = splitOn ":" (attachdata !! 0) !! 1
      let key  = attachdata !! 1 
          
      return $ SF.joinPath [ key, path]

  

--itemData :: HDBC.IConnection conn => conn -> Int -> IO [(String, String)

itemData ::  Int -> DBConn [(String, String)] 
itemData itemID = do   
  
  let itemID' = fromIntToInt64 itemID 

  sqlQueryAll  sql [HDBC.SqlInt64 itemID'] projection

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
itemAuthors :: Int -> DBConn [[String]]
itemAuthors itemID = do 

  let itemID' = fromIntToInt64 itemID

  sqlQueryAll sql [HDBC.SqlInt64 itemID'] (map fromSqlToString)

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

getZoteroItemJSON :: Int -> DBConn BLI.ByteString  
getZoteroItemJSON itemID = do 
  zitem <- getZoteroItem  itemID
  return $ encode zitem 


getZoteroItemsJSON :: [Int] -> DBConn BLI.ByteString
getZoteroItemsJSON itemIDs = do
  zitems <- mapM getZoteroItem itemIDs
  return $ encode zitems 
--  encode <$> mapM (getZoteroItem conn) itemIDs 

{-
getCollectionItemsJSON conn collID =
  collectionItems conn collID >>= getZoteroItemsJSON conn
-}

-- getCollectionItemsJSON conn collID =
--   runReaderT (collectionItems collID) conn
--   >>= getZoteroItemsJSON conn

getCollectionItemsJSON :: Int -> DBConn BLI.ByteString
getCollectionItemsJSON collID = do
  conn               <- ask 
  itemIDs            <- collectionItems collID  
  getZoteroItemsJSON itemIDs 


getTagItems :: Int -> DBConn [Int]
getTagItems tagID = do 
  
  let tagID' = fromIntToInt64 tagID 

  sqlQueryColumn sql [HDBC.SqlInt64 tagID'] fromSqlToInt
                                        
  where
    sql = unlines $ ["SELECT itemID", 
                     "FROM itemTags", 
                     "WHERE tagID = ?"
                     ]

-- getTagItemsJSON :: HDBC.IConnection conn => conn -> Int -> IO BLI.ByteString
-- getTagItemsJSON conn tagID =
--   runReaderT (getTagItems tagID) conn
--   >>= getZoteroItemsJSON conn

getTagItemsJSON :: Int -> DBConn BLI.ByteString
getTagItemsJSON tagID = do
  itemIDs <- getTagItems tagID
  getZoteroItemsJSON itemIDs 


getAuthors :: DBConn [ZoteroAuthor]
getAuthors  = do
  
  sqlQueryAll sql [] projection 

  where

    sql = "SELECT creatorDataID, firstName, lastName FROM creatorData"
  
    projection row = ZoteroAuthor (fromSqlToInt    (row !! 0))
                                  (fromSqlToString (row !! 1))
                                  (fromSqlToString (row !! 2))

getAuthorsJSON :: DBConn BLI.ByteString
getAuthorsJSON = 
  encode <$> getAuthors 


getItemsFromAuthor :: Int -> DBConn [Int]
getItemsFromAuthor  authorID =

  let authorID' = fromIntToInt64 authorID in
  
  sqlQueryColumn  sql [HDBC.SqlInt64 authorID'] fromSqlToInt

  where

    sql = "SELECT itemID FROM itemCreators WHERE creatorID = ?"


getItemsFromAuthorJSON :: Int -> DBConn BLI.ByteString
getItemsFromAuthorJSON authorID = do 
  itemIDs <- getItemsFromAuthor authorID
  getZoteroItemsJSON itemIDs


getRelatedTags :: Int -> DBConn [ZoteroTag]
getRelatedTags tagID = do 

  let tagID' = fromIntToInt64 tagID

  sqlQueryAll sql [HDBC.SqlInt64 tagID', HDBC.SqlInt64 tagID'] projection  

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

getRelatedTagsJSON :: Int -> DBConn BLI.ByteString
getRelatedTagsJSON tagID = do 
  tags <- getRelatedTags tagID
  return $ encode tags


getTagsFromCollection :: Int -> DBConn [ZoteroTag] 
getTagsFromCollection  collID = do 

  let collID' = fromIntToInt64 collID 

  sqlQueryAll sql [HDBC.SqlInt64 collID'] projection 
  
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


getTagsFromCollectionJSON :: Int ->  DBConn BLI.ByteString
getTagsFromCollectionJSON collID = 
  encode <$> getTagsFromCollection collID


searchByTitleWordLike :: String -> DBConn [Int]
searchByTitleWordLike  searchWord = do
  
  sqlQueryColumn sql [HDBC.SqlString searchWord]  fromSqlToInt
    
    where

      sql = unlines $ [

        "SELECT itemData.itemID"  
       ,"FROM   itemData, itemDataValues, itemAttachments"
       ,"WHERE  fieldID = 110" 
       ,"AND    itemData.valueID = itemDataValues.valueID"
       ,"AND    itemAttachments.sourceItemID = itemData.itemID"
       ,"AND    itemDataValues.value LIKE ?"      
       ]

searchByTitleWordLikeJSON :: String -> DBConn BLI.ByteString
searchByTitleWordLikeJSON searchWord = do  
  items <- searchByTitleWordLike searchWord  
  getZoteroItemsJSON items 

    

{-

:load zotero.hs
conn <- dbConnection
coll <- head <$>  getCollections conn

 row <-  sqlQuery conn "SELECT * FROM collections" [] 

-}
