{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

{- |
Module      : Zotero
Description : Interface to Zotero Database
License     : Public Domain


Interface to Zotero database to query and manipulate the database.

-}
module Zotero 
       (
         -- * Types 
         DBConn
        ,ZoteroItem    (..)
        ,ZoteroAuthor  (..)
        ,ZoteroTag     (..)
        ,ZoteroColl    (..)

         -- Type Aliases 
        ,ZoteroItemID
        ,ZoteroTagID
        ,ZoteroTagName
        ,ZoteroItemTags
        ,ZoteroItemMime
       
         -- * Functions 
         ,withConnection
         ,getCollections
       --  ,showCollections
         ,collectionItems
         ,itemTagsData
         ,itemTags
         ,itemData
         ,itemAttachmentData
         ,itemAttachmentFile
         ,itemAuthors
         ,sqlQuery
         ,sqlQueryAll
         ,sqlQueryRow
      
         ,dbConnection

         ,getTagItems
         ,getRelatedTags
         ,getTagsFromCollection
         ,searchByTitleWordLike
         ,searchByContentAndTitleLike

          ,getCollectionChild
          ,getCollectionTop

           , itemsWithoutCollections

          {- JSON Export Functions -}
         ,getCollectionsJSON
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
         ,searchByContentAndTitleLikeJSON
         ,getCollectionChildJSON
         ,getCollectionTopJSON
         ,itemsWithoutCollectionsJSON
         ,getZoteroItemIdAsListJSON

          
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


import System.Random (getStdGen, newStdGen, randomRs)

{- ---------------------- Types -----------------}

type SQLQuery =  [(String, HDBC.SqlValue)] 

{- | Database Connection -> DbConn a = ReaderT conn IO a = conn -> IO a -}
type DBConn a = forall conn. (HDBC.IConnection conn) =>  ReaderT conn IO a

{- | Zotero Tag ID number -} 
type ZoteroTagID   = Int

{- | Tag Name  -}
type ZoteroTagName = String


type ZoteroItemID     = Int
type ZoteroItemString = String
type ZoteroItemTags   = [(ZoteroTagID, ZoteroTagName)]
type ZoteroItemMime   = String

{- | ZoteroItem  data -}
data ZoteroItem =
  ZoteroItem {    zoteroItemID          :: Int
                , zoteroItemData        :: [(String, String)]
                , zoteroItemAuthors     :: [ZoteroAuthor]  -- (AuthorID, [firstName, lastName]) 
                , zoteroItemTags        :: [(Int, String)] -- (tagID, tag)
                , zoteroItemCollections :: [(Int, String)] -- (collID, collection)
                                           
                , zoteroItemFile        :: Maybe String    -- File attachment
                , zoteroItemMime        :: Maybe String    -- Mime Type
                
             } deriving (Eq, Show, Read,  Generic)



instance FromJSON ZoteroItem

instance ToJSON ZoteroItem where
  toJSON (ZoteroItem itemID itemData itemAuthors itemTags 
          itemColls itemFile itemMime) = object
    [
       "id"       .= itemID
      ,"data"     .= itemData       
      ,"authors"  .= itemAuthors
      ,"tags"     .= itemTags
      ,"colls"    .= itemColls
      ,"file"     .= itemFile
      ,"mime"     .= itemMime
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

instance FromJSON ZoteroAuthor    


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

stripPrefixStr :: String -> String -> String 
stripPrefixStr prefix str =
  case T.unpack <$> T.stripPrefix (T.pack prefix) (T.pack str) of
    Nothing -> str
    Just s  -> s 
 

-- dbConnection = SQLite.connectSqlite3 database
dbConnection = PgSQL.connectPostgreSQL "postgres://postgres@localhost/zotero"



{- ================== Helper Functions ======================  -}

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

createKey :: IO String 
createKey = do
  g <- newStdGen
  return $ map (alphabet!!) $ take 8 $ randomRs (0, 35) g
  where
    alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"


(<!!>) :: [a] -> Int -> Maybe a
(<!!>) xs i =
  if length xs > i
  then Just $ xs !! i
  else Nothing

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
  return (fmap projection rows)


{-
sqlQueryRow :: HDBC.IConnection conn => conn
               -> String
               -> [HDBC.SqlValue]
               -> (HDBC.SqlValue -> b)
               -> IO [b]
-}

sqlQueryRow :: String -> [HDBC.SqlValue] -> (HDBC.SqlValue -> b) -> DBConn [b]   
sqlQueryRow sql sqlvals coercion = do
  sqlQueryAll sql sqlvals (coercion . (!!0))


sqlQueryOne :: String -> [HDBC.SqlValue] -> (HDBC.SqlValue -> b) -> DBConn (Maybe b)
sqlQueryOne sql sqlvals projection = do
  conn   <- ask 
  stmt   <- liftIO $  HDBC.prepare conn sql
  liftIO $ HDBC.execute stmt sqlvals
  row     <- liftIO $ HDBC.fetchRow stmt
  liftIO $ HDBC.commit conn 
  return $ (!!0) . (map projection) <$> row

  
  

sqlRun :: String -> [HDBC.SqlValue] -> DBConn ()
sqlRun sql sqlvals = do
  conn    <- ask 
  stmt    <- liftIO $ HDBC.prepare conn sql
  liftIO  $ HDBC.execute stmt sqlvals
  liftIO  $ HDBC.commit conn 
    
        
withConnection :: HDBC.IConnection  conn => IO conn -> (conn -> IO r) -> IO r
withConnection ioConn function = do
  conn     <- ioConn
  result   <- function conn
  return result



{- ================== Database Functions  ======================  -}



{- | getZoteroItem - Get Zotero Item from the database -}
getZoteroItem :: ZoteroItemID -> DBConn ZoteroItem
getZoteroItem itemID = do

  itemData    <- itemData itemID
  itemAuthors <- itemAuthors itemID
  itemTags    <- itemTagsData itemID
  itemColls   <- itemCollections itemID
  itemFile    <- itemAttachmentFile itemID
  itemMime    <- return Nothing

  return $ ZoteroItem itemID
                      itemData
                      itemAuthors
                      itemTags
                      itemColls
                      itemFile
                      itemMime


-- getCollections :: HDBC.IConnection conn => conn -> IO [(Int, String)]

{- | Get all collections -}
getCollections :: DBConn [ZoteroColl]
getCollections = do

  sqlQueryAll sql [] projection
  
    where
      
      sql = "SELECT collectionID, collectionName \
            \FROM collections \
            \ORDER BY collectionName"
            
        
      projection  row =  ZoteroColl (fromSqlToInt $ row !! 0)
                                    (fromSqlToString $ row !! 1)

{- | Get all collections as JSON -}
getCollectionsJSON :: DBConn BLI.ByteString
getCollectionsJSON = encode <$> getCollections

{- | Get only top level collections -}
getCollectionTop :: DBConn [ZoteroColl]
getCollectionTop = do
  sqlQueryAll sql [] projection
    where
      sql = "SELECT collectionID, collectionName FROM collections \
            \WHERE  parentCollectionID IS NULL"

      projection  row =  ZoteroColl (fromSqlToInt $ row !! 0)
                                    (fromSqlToString $ row !! 1)        


getCollectionTopJSON :: DBConn BLI.ByteString
getCollectionTopJSON = encode <$> getCollectionTop

{- | Get sub-collections of a collection -}
getCollectionChild :: Int -> DBConn [ZoteroColl]
getCollectionChild collID = do

  let collID' = fromIntToInt64 collID
  
  sqlQueryAll sql [HDBC.SqlInt64 collID'] projection  
  
    where
  
      sql = "SELECT collectionID, collectionName FROM collections \
            \WHERE  parentCollectionID = ?"  

      projection  row =  ZoteroColl (fromSqlToInt $ row !! 0)
                                    (fromSqlToString $ row !! 1)
        

getCollectionChildJSON :: Int -> DBConn BLI.ByteString
getCollectionChildJSON collID = encode <$> getCollectionChild collID

--showCollections :: DBConn ()  
-- showCollections conn = do
--   mapM_ print =<< runReaderT getCollections conn 

-- showCollections2 :: DBConn ()
-- showCollections2 = do
--   colls <- getCollections
--   liftIO  (mapM_ print colls)


--collectionItems :: HDBC.IConnection conn => conn -> Int -> IO [Int]
collectionItems :: Int -> DBConn [Int]
collectionItems collID = do
  
  let collID' = fromIntToInt64 collID
  sqlQueryRow sql [HDBC.SqlInt64 collID'] fromSqlToInt
  
  where
    sql = "SELECT  itemID FROM collectionItems WHERE collectionID = ?"

{- Returns all tags of a given item -}
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

{- Returns all collections that an item benlongs to

  itemCollections :: itemID -> [(Collection ID, Collection Name)]

-}
itemCollections :: Int -> DBConn [(Int, String)]
itemCollections  itemID = do

    let itemID' = fromIntToInt64 itemID    
    sqlQueryAll sql [HDBC.SqlInt64 itemID'] projection

    where
      
      projection xs = (fromSqlToInt (xs !! 0), fromSqlToString (xs !! 1))

      sql = unlines $ [ "SELECT collectionItems.collectionID, collections.collectionName"
                       ,"FROM   collectionItems, collections"                        
                       ,"WHERE  itemID = ?"
                       ,"AND    collectionItems.collectionID = collections.collectionID"
                      ]


{- | Return all zotero items without collections -}
itemsWithoutCollections :: Int -> Int -> DBConn [ZoteroItemID]
itemsWithoutCollections paging offset  =
  
  sqlQueryRow sql [HDBC.SqlInt64 $ fromIntToInt64 paging,
                   HDBC.SqlInt64 $ fromIntToInt64 (offset * paging)
                  ]
                  fromSqlToInt  
  where
            
    sql = "SELECT itemID \
          \FROM   items \
          \WHERE  itemID NOT IN ( SELECT itemID FROM collectionItems ) \
          \ORDER BY itemID \
          \LIMIT ? \
          \OFFSET ? "

itemsWithoutCollectionsJSON :: Int -> Int -> DBConn BLI.ByteString
itemsWithoutCollectionsJSON paging offset = do 
  items <- itemsWithoutCollections paging offset 
  json  <- getZoteroItemsJSON items
  return json 
  

-- Return all tags in the database
-- 
getTags :: DBConn [ZoteroTag]
getTags = do
  sqlQueryAll sql [] projection
  where
    sql =   "SELECT tagID, name FROM tags \
           \ORDER BY name"

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



{-
    ["/home/tux/Downloads/Python Packaging for Production.pdf",
     "7ZPVKJQH","application/pdf",
     "attachment"
     ]

-}

itemAttachmentFile :: Int -> DBConn (Maybe FilePath)
itemAttachmentFile itemID = do
  
  attachmentData <- itemAttachmentData itemID
  return $  attachmetFile attachmentData

  where
    
      -- Maybe Monad
    attachmetFile :: Maybe [String] -> Maybe String 
    attachmetFile attachmentData = do
      
      attachdata <- attachmentData      
      key        <- attachdata <!!> 1
      path'      <- attachdata <!!> 0      
--      path       <- (splitOn ":" path') <!!> 1
      return $ SF.joinPath [ key, stripPrefixStr "storage:"  path']

        
      -- return $ do path <- splitOn ":" (attachdata <!!> 0) >>= (<!!>1)
      --             key  <-  attachdata <!!> 1 
      --             return $ SF.joinPath [ key, path]

  

--itemData :: HDBC.IConnection conn => conn -> Int -> IO [(String, String)

itemData ::  ZoteroItemID -> DBConn [(String, String)]
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
itemAuthors :: ZoteroItemID -> DBConn [ZoteroAuthor]
itemAuthors itemID = do 

  let itemID' = fromIntToInt64 itemID

  sqlQueryAll sql [HDBC.SqlInt64 itemID'] projection 

  where

    projection row = ZoteroAuthor (fromSqlToInt    (row !! 0))
                                  (fromSqlToString (row !! 1))
                                  (fromSqlToString (row !! 2))

    sql = unlines $ [
      
      "SELECT   itemCreators.creatorID, creatorData.firstName, creatorData.lastName",
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

getZoteroItemJSON :: ZoteroItemID -> DBConn BLI.ByteString
getZoteroItemJSON itemID = do 
  zitem <- getZoteroItem  itemID
  return $ encode zitem 


getZoteroItemsJSON :: [ZoteroItemID] -> DBConn BLI.ByteString
getZoteroItemsJSON itemIDs = do
  zitems <- mapM getZoteroItem itemIDs
  return $ encode zitems 


getZoteroItemIdAsListJSON :: ZoteroItemID -> DBConn BLI.ByteString
getZoteroItemIdAsListJSON itemID = getZoteroItemsJSON [itemID]

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


{- | Get all items IDs that belong to a given tag specified by its Id -}
getTagItems :: Int -> DBConn [ZoteroItemID]
getTagItems tagID = do 
  let tagID' = fromIntToInt64 tagID 
  sqlQueryRow sql [HDBC.SqlInt64 tagID'] fromSqlToInt
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

    sql = "SELECT    creators.creatorID, creatorData.firstName, creatorData.lastName \
          \FROM      creatorData, creators \
          \WHERE     creatorData.creatorDataID = creators.creatorDataID \
          \ORDER BY creatorData.firstName || ' ' || creatorData.lastName"
  
    projection row = ZoteroAuthor (fromSqlToInt    (row !! 0))
                                  (fromSqlToString (row !! 1))
                                  (fromSqlToString (row !! 2))

getAuthorsJSON :: DBConn BLI.ByteString
getAuthorsJSON = 
  encode <$> getAuthors 


getItemsFromAuthor :: Int -> DBConn [Int]
getItemsFromAuthor  authorID =

  let authorID' = fromIntToInt64 authorID in
  
  sqlQueryRow  sql [HDBC.SqlInt64 authorID'] fromSqlToInt

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
  
  sqlQueryRow sql [HDBC.SqlString searchWord]  fromSqlToInt
    
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

{- Search all items which content (word) and title
   contains a search string.
-}
searchByContentAndTitleLike :: String -> DBConn [Int]
searchByContentAndTitleLike searchWord = do

  sqlQueryRow sql [HDBC.SqlString searchWord, HDBC.SqlString searchWord]  fromSqlToInt
  
  where
     sql = unlines $
       [
        "SELECT itemData.itemID"
       ,"FROM   itemData, itemDataValues, fulltextItemWords, fulltextWords "
       ,"WHERE  itemData.fieldID = 110" 
       ,"AND    itemDataValues.valueID = itemData.valueID"
       ,"AND    fulltextItemWords.wordID = fulltextWords.wordID"
       ,"AND    fulltextItemWords.itemID = itemData.itemID"
       ,"AND    (itemDataValues.value LIKE ?"
       ,"           OR                      "
       ,"       fulltextWords.word LIKE ?)"
       ]
                                           
searchByContentAndTitleLikeJSON :: String -> DBConn BLI.ByteString  
searchByContentAndTitleLikeJSON searchWord = do
  items <- searchByContentAndTitleLike searchWord  
  getZoteroItemsJSON items 




replaceTagBy :: Int -> Int -> DBConn ()
replaceTagBy  tagIDfrom tagIDto = do

  let tagIDfrom' = fromIntToInt64 tagIDfrom
  let tagIDto'   = fromIntToInt64 tagIDto

  sqlRun sql1 [HDBC.SqlInt64 tagIDto', HDBC.SqlInt64 tagIDfrom']
  sqlRun sql2 [HDBC.SqlInt64 tagIDfrom']
    
  where

    sql1 = "UPDATE itemTags    \
           \SET    tagID = ?   \
           \WHERE  tagID = ? "

    sql2 = "DELETE FROM tags WHERE tagID = ?"



removeAuthor :: Int -> DBConn ()
removeAuthor id = do 
  let id' = fromIntToInt64 id
  row  <-  sqlQuery sql0 [HDBC.SqlInt64 id'] (\x -> x)
  let dataId = fmap head row
                     
  case dataId of
    Nothing      -> return ()
    Just dataId' -> do
                        sqlRun sql1 [HDBC.SqlInt64 id']
                        sqlRun sql2 [HDBC.SqlInt64 id']
                        sqlRun sql3 [dataId']

  where

    sql0 = "SELECT creatorDataID FROM creators WHERE creatorID = ?"
    
    sql1 = "DELETE FROM itemCreators WHERE creatorID = ? "

    sql2 = "DELETE FROM creators WHERE creatorID = ?"

    sql3 = "DELETE FROM creatorData WHERE creatorDataID = ?"
    

replaceAuthorBy :: Int -> Int -> DBConn ()
replaceAuthorBy fromId toId = do

  let fromId' = fromIntToInt64 fromId
  let toId'   = fromIntToInt64 toId

  sqlRun sql [HDBC.SqlInt64 toId', HDBC.SqlInt64 fromId']

  removeAuthor fromId
  
  where

    sql = "UPDATE itemCreators \
          \SET    creatorID = ? \
          \WHERE  creatorID = ? "


renameTag :: Int -> String -> DBConn ()
renameTag id name = do
  let id' = fromIntToInt64 id
  sqlRun sql [HDBC.SqlString name, HDBC.SqlInt64 id']
  where
    sql = "UPDATE tags \
          \SET    name = ? \
          \WHERE  tagID = ?"



renameCollection :: Int -> String -> DBConn ()
renameCollection id name = do
  let id' = fromIntToInt64 id
  sqlRun sql [HDBC.SqlString name, HDBC.SqlInt64 id']
  where
    sql = "UPDATE collections \
          \SET    collectionName = ? \
          \WHERE  collectionID = ?"
          

deleteTag :: Int -> DBConn ()
deleteTag id = do
  
  let id' = fromIntToInt64 id

  sqlRun sql1 [HDBC.SqlInt64 id']
  sqlRun sql2 [HDBC.SqlInt64 id']

  where
    sql1 = "DELETE  FROM itemTags WHERE tagID = ?"
    sql2 = "DELETE  FROM tags WHERE tagID = ?"


renameAuthor :: Int -> String -> String -> DBConn ()
renameAuthor id firstName lastName = do
  
  let id' = fromIntToInt64 id

  sqlRun sql [HDBC.SqlString firstName, HDBC.SqlString lastName, HDBC.SqlInt64 id']

  where

    sql = "UPDATE creatorData \
          \SET   firstName = ?, lastName = ? \
          \WHERE  creatorDataID IN \
          \         (SELECT creatorData.creatorDataID \
          \          FROM creatorData, creators \
          \          WHERE  creators.creatorDataID  = creatorData.creatorDataID \
          \          AND    creators.creatorID = ? )"



setItemField :: Int -> String -> String -> DBConn () 
setItemField itemID field value  = do

  let itemID' = fromIntToInt64 itemID

  sqlRun sql [HDBC.SqlString value,
              HDBC.SqlInt64  itemID',
              HDBC.SqlString field]

  where

    sql = "UPDATE itemDataValues \
          \SET    value = ? \ 
          \WHERE  valueID IN ( \
          \SELECT  itemDataValues.valueID \
          \FROM    fields, itemDataValues, itemData \
          \WHERE   itemData.fieldID = fields.fieldID \
          \AND     itemData.valueID = itemDataValues.valueID \
          \AND     itemData.itemID = ? \
          \AND     fields.fieldName = ? )"



{- | Note: Only works in PostgresSQL -}
insertCollection :: String -> DBConn Int 
insertCollection name = do
  key <- liftIO $ createKey
  id <- fromJust <$> sqlQueryOne sql [HDBC.SqlString name, HDBC.SqlString key] fromSqlToInt
  return id 
  where
    sql = "WITH rows as (\
          \INSERT INTO collections (collectionID, collectionName, key)\
          \VALUES ( (SELECT 1 + max(collectionID) FROM collections), ?, ?)\
          \)\
          \SELECT max(collectionID) FROM collections"




{-

:load zotero.hs
conn <- dbConnection
coll <- head <$>  getCollections conn

 row <-  sqlQuery conn "SELECT * FROM collections" [] 

-}
