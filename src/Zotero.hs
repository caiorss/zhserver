{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

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
        ,HDBConn (..)
        ,DBUri   (..)

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


        ,openDBConnection
        ,runDBConn
        ,withDBConnection
        ,withDBConnection2

         -- -----------------------
         ,withConnection
         ,getCollections
       --  ,showCollections
         ,collectionItems
         ,getItemTagsData
         ,getItemTags
         ,getItemData
         ,getItemAttachmentData
         ,getItemAttachmentFile
         ,getItemAuthors
         ,sqlQuery
         ,sqlQueryAll
         ,sqlQueryRow
      

         ,getTags
         ,getTagItems
         ,getRelatedTags
         ,getTagsFromCollection
         ,searchByTitleWordLike
         ,searchByContentAndTitleLike
         ,searchByTitleTags

          ,getCollectionChild
          ,getCollectionsTop

          ,itemsWithoutCollections

          ,getSubcollections
          ,getSubcollectionsIDNames
          ,getAllSubCollections
          ,getAllSubCollectionsItems

       
          ,renameTag
          ,mergeTags
          ,addTagsToItem

          ,getTagName
          ,getCollName
          ,getAuthorName

          ,searchTag
          ,searchCollection

           -- Item fields -------
           --
          ,getItemURL
          ,getItemISBN
          ,getItemISSN
          ,getItemDOI
          ,getItemTitle

           -- Update Functions -----
           --
           ,updateItemDataField
           ,updateItemTitle
           ,updateItemDate
           ,updateItemAbstract
           ,updateItemISSN
           ,updateItemISBN
           ,updateItemDOI

           ,insertItem
           ,insertField
           ,insertItemID
           ,deleteItem

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
         ,getCollectionsTopJSON
         ,itemsWithoutCollectionsJSON
         ,getZoteroItemIdAsListJSON

         ,getItemsFromAuthor

         ,searchByTitleTagsAndInWords
         ,searchByTitleTagsOrInWords
         ,getAuthors         

       ) where


import Control.Monad.Trans
import Control.Monad.Trans.Reader

import Data.Maybe (catMaybes, maybe, fromJust, fromMaybe)
import Data.List (lookup)
import Control.Monad

import qualified Database.HDBC.Sqlite3 as Sqlite3
import qualified Database.HDBC.PostgreSQL as Pg
import qualified Database.HDBC as HDBC

-- import qualified Database.HDBC.PostgreSQL as Pg
-- import qualified Database.HDBC.Sqlite3    as Sqlite3


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

import qualified Text.Printf as P
import qualified Data.Text as T

import qualified System.Environment as Env

import System.Random (getStdGen, newStdGen, randomRs)    
import DBUtils    



-- withDBConnection2 dbUri dbAction = do
--   withDBConnection dbUri (ioToDBConn dbAction)

{- | Zotero Tag ID number -} 
type ZoteroTagID   = Int

{- | Tag Name  -}
type ZoteroTagName = String

{-| Item ID type Alias -}
type ZoteroItemID     = Int

type ZoteroItemString = String
type ZoteroItemTags   = [(ZoteroTagID, ZoteroTagName)]
type ZoteroItemMime   = String

{- | Collection ID type alias -}
type ZoteroCollectionID = Int

type ZoteroFieldID = Int
type ZoteroFieldValue = String


{- | ZoteroItem  data -}
data ZoteroItem =
  ZoteroItem {    zoteroItemID          :: Int                  --  Item ID number
                , zoteroItemType        :: String 
                , zoteroItemData        :: [(String, String)]   --  Item data (key, value) pair list
                , zoteroItemAuthors     :: [ZoteroAuthor]       --  (AuthorID, [firstName, lastName])
                , zoteroItemTags        :: [(Int, String)]      --  (tagID, tag)
                , zoteroItemCollections :: [(Int, String)]      --  (collID, collection)
                , zoteroItemFile        :: Maybe String         --  File attachment
                , zoteroItemMime        :: Maybe String         --  Mime Type
                
             } deriving (Eq, Show, Read,  Generic)



instance FromJSON ZoteroItem

instance ToJSON ZoteroItem where
  toJSON (ZoteroItem itemID itemType itemData itemAuthors itemTags 
          itemColls itemFile itemMime) = object
    [
       "id"       .= itemID
      ,"type"     .= itemType            
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
 

{- ================== Helper Functions ======================  -}


makeKey :: IO String
makeKey = do
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


{- ================== Database Functions  ======================  -}



{- | getZoteroItem - Get all Zotero Item data from the database -}
getZoteroItem :: ZoteroItemID -> DBConn ZoteroItem
getZoteroItem itemID = do
  itemType    <- getItemType itemID
  itemData    <- getItemData itemID
  itemAuthors <- getItemAuthors itemID
  itemTags    <- getItemTagsData itemID
  itemColls   <- getItemCollections itemID
  itemFile    <- getItemAttachmentFile itemID
  itemMime    <- return Nothing

  return $ ZoteroItem itemID
                      itemType
                      itemData
                      itemAuthors
                      itemTags
                      itemColls
                      itemFile
                      itemMime


getItemType :: Int -> DBConn String
getItemType itemID = fromJust <$> sqlQueryOne sql [fromIntToHDBC itemID] fromSqlToString
  where
    sql = unlines [
           "SELECT itemTypes.typeName FROM itemTypes, items"
          ,"WHERE items.itemID = ?"
          ,"AND   items.itemTypeID = itemTypes.itemTypeID"
          ]


{- | Get name of tag given its ID -}
getTagName :: ZoteroTagID -> DBConn (Maybe String)
getTagName tagID = sqlQueryOne sql [HDBC.SqlInt64 $  fromIntToInt64 tagID] fromSqlToString
  where
    sql = "SELECT name FROM tags WHERE tagID = ?"

{- | Get name of collection given its ID -}
getCollName :: Int -> DBConn (Maybe String)
getCollName collID = sqlQueryOne sql [HDBC.SqlInt64 $  fromIntToInt64 collID] fromSqlToString
  where
    sql = "SELECT collectionName FROM collections WHERE collectionID = ?"

{- | Get Author Name -}
getAuthorName ::  Int -> DBConn (Maybe String)
getAuthorName = undefined

{- | Get all collections data. -}
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
getCollectionsTop :: DBConn [ZoteroColl]
getCollectionsTop = do
  sqlQueryAll sql [] projection
    where
      sql = "SELECT collectionID, collectionName FROM collections \
            \WHERE  parentCollectionID IS NULL"

      projection  row =  ZoteroColl (fromSqlToInt $ row !! 0)
                                    (fromSqlToString $ row !! 1)        


getCollectionsTopJSON :: DBConn BLI.ByteString
getCollectionsTopJSON = encode <$> getCollectionsTop

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

{- | Query all items from a collection defined by its ID. -}
collectionItems :: ZoteroCollectionID -> DBConn [ZoteroItemID]
collectionItems collID = do
  let collID' = fromIntToInt64 collID
  sqlQueryRow sql [HDBC.SqlInt64 collID'] fromSqlToInt
  where
    sql = "SELECT  itemID FROM collectionItems WHERE collectionID = ?"

{- | Returns all tags of a given item -}
getItemTagsData :: ZoteroItemID -> DBConn ZoteroItemTags
getItemTagsData itemID = do
  let itemID' = fromIntToInt64 itemID 
  sqlQueryAll sql [HDBC.SqlInt64 itemID'] projection
  where
    sql = unlines ["SELECT  tags.tagID, tags.name",
                   "FROM    itemTags, tags", 
                   "WHERE   itemTags.tagID = tags.tagID", 
                   "AND     itemID = ?"
                   ]

    projection xs = (fromSqlToInt (xs !! 0), fromSqlToString (xs !! 1))


{- | Return only the tag names of a given itemID  -}
getItemTags :: ZoteroItemID -> DBConn [String]
getItemTags itemID =
  map snd <$> getItemTagsData itemID 

{-| Returns all collections that an item benlongs to

   itemCollections :: itemID -> [(Collection ID, Collection Name)]
-}
getItemCollections :: Int -> DBConn [(Int, String)]
getItemCollections  itemID = do

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
  
{- | Get all sub collections from a parent collection. -}
getSubcollections :: ZoteroCollectionID -> DBConn [ZoteroCollectionID]
getSubcollections collID = do
  sqlQueryRow sql [HDBC.SqlInt64 $ fromIntToInt64 collID] fromSqlToInt
  where
    sql = unlines [ "SELECT collectionID FROM collections"
                  ,"WHERE  parentCollectionID = ?"
                  ]
{- | Get subcollecotions ID and Name from a parent collection. -}
getSubcollectionsIDNames :: ZoteroCollectionID -> DBConn [(Int, String)]
getSubcollectionsIDNames collID = do
  sqlQueryAll sql [HDBC.SqlInt64  $ fromIntToInt64 collID ] projection
  where
    projection row = (fromSqlToInt (row !! 0) , fromSqlToString (row !! 1))

    sql = unlines [ "SELECT collectionID, collectionName FROM collections"
                  ,"WHERE  parentCollectionID = ?"
                  ]

{- | @IN-PROGRESS -}
mapconcatM :: Monad m => (a -> m [b]) -> [a] -> m [b]
mapconcatM fn xs = aux fn xs []
  where
    aux fn xs acc =
      case xs of
        []   -> return acc
        y:ys -> do blist <- fn y
                   acc `seq` aux fn ys (blist ++ acc)

getAllSubCollections :: ZoteroCollectionID -> DBConn [(Int, String)]
getAllSubCollections collID  = do
  subcolls <- getSubcollectionsIDNames collID
  colls    <- mapconcatM (\(cID, cName) -> getAllSubCollections cID) subcolls
  return $ subcolls ++ colls


getAllSubCollectionsItems :: ZoteroCollectionID -> DBConn [ZoteroItemID]
getAllSubCollectionsItems collID = do
  subcolls     <- getAllSubCollections collID
  items        <- collectionItems collID
  subcollItems <- mapconcatM (\ (cID, cName) -> collectionItems cID) subcolls
  return $ items ++ subcollItems

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

{- | Get Zotero item attachment file. -}
getItemAttachmentData :: ZoteroItemID -> DBConn (Maybe [String])
getItemAttachmentData itemID = do 
  
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



getItemAttachmentFile :: Int -> DBConn (Maybe FilePath)
getItemAttachmentFile itemID = do
  
  attachmentData <- getItemAttachmentData itemID
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

getItemData ::  ZoteroItemID -> DBConn [(String, String)]
getItemData itemID = do   
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
getItemAuthors :: ZoteroItemID -> DBConn [ZoteroAuthor]
getItemAuthors itemID = do 

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


getZoteroItemJSON :: ZoteroItemID -> DBConn BLI.ByteString
getZoteroItemJSON itemID = do 
  zitem <- getZoteroItem  itemID
  return $ encode zitem 

{- | Get a list of zotero item data as json given its IDs -}
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

getTagItemsJSON :: ZoteroTagID -> DBConn BLI.ByteString
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
          \ORDER BY  creatorData.firstName || ' ' || creatorData.lastName"
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


getRelatedTags :: ZoteroTagID -> DBConn [ZoteroTag]
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

getRelatedTagsJSON :: ZoteroTagID -> DBConn BLI.ByteString
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



searchTag :: String -> DBConn [(ZoteroTagID, ZoteroTagName)]
searchTag = makeSearchNameIdFun "SELECT tagID, name FROM tags WHERE name LIKE ?"


searchCollection :: String -> DBConn [(Int, String)]
searchCollection = makeSearchNameIdFun "SELECT collectionID, collectionName FROM collections WHERE collectionName LIKE ?"



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

{- | Search all items which title 'or' tag matches all words in a given list -}
searchByTitleTagsAndInWords :: [String] -> DBConn [ZoteroItemID]
searchByTitleTagsAndInWords words = do
  sqlQueryRow (P.printf sql subquery) [] fromSqlToInt  
  where
    tpl word = P.printf "(itemDataValues.value LIKE \"%%%s%%\" OR tags.Name LIKE \"%%%s%%\")" word word 
    subquery = joinStrings " AND "  (map tpl  words)
    sql = unlines $ [
                   "SELECT itemData.itemID",
                   "FROM   itemData, itemDataValues, itemAttachments, tags, itemTags",
                   "WHERE  fieldID = 110",
                   "AND    itemData.valueID = itemDataValues.valueID",
                   "AND    itemAttachments.sourceItemID = itemData.itemID",
                   "AND    itemTags.itemID = itemData.itemID",
                   "AND    itemTags.tagID = tags.tagID",
                   "AND    (  %s  )",
                   "GROUP BY itemData.itemID"                    
                    ]

searchByTitleTags :: String -> DBConn [ZoteroItemID]
searchByTitleTags word = do
  sqlQueryRow sql [HDBC.SqlString word, HDBC.SqlString word] fromSqlToInt
  where
    sql = unlines $ [
                   "SELECT itemData.itemID",
                   "FROM   itemData, itemDataValues, itemAttachments, tags, itemTags",
                   "WHERE  fieldID = 110",
                   "AND    itemData.valueID = itemDataValues.valueID",
                 --  "AND    itemAttachments.sourceItemID = itemData.itemID",
                   "AND    itemTags.itemID = itemData.itemID",
                   "AND    itemTags.tagID = tags.tagID",
                   "AND (LOWER(itemDataValues.value) LIKE ? OR LOWER(tags.Name) LIKE ?)",
                   "GROUP BY itemData.itemID"
                    ]




{- | Search all items for which title 'or' tag matches at least one words in a given list -}
searchByTitleTagsOrInWords :: [String] -> DBConn [ZoteroItemID]
searchByTitleTagsOrInWords words = do
  sqlQueryRow (P.printf sql subquery) [] fromSqlToInt
  where
    tpl word = P.printf "(itemDataValues.value LIKE \"%%%s%%\" OR tags.Name LIKE \"%%%s%%\")" word word
    subquery = joinStrings " OR "  (map tpl  words)
    sql = unlines $ [
                   "SELECT itemData.itemID, itemDataValues.value",
                   "FROM   itemData, itemDataValues, itemAttachments, tags, itemTags",
                   "WHERE  fieldID = 110",
                   "AND    itemData.valueID = itemDataValues.valueID",
                   "AND    itemAttachments.sourceItemID = itemData.itemID",
                   "AND    itemTags.itemID = itemData.itemID",
                   "AND    itemTags.tagID = tags.tagID",
                   "AND    (  %s  )",
                   "GROUP BY itemData.itemID"
                    ]

  

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
    -- Remove all rows from itemTags table where tagID = tag to be removed 
    sql1 = "DELETE  FROM itemTags WHERE tagID = ?"
    -- Remove the tag where tagID = tag to be Removed.
    sql2 = "DELETE  FROM tags WHERE tagID = ?"


{- | Move all items which has tag1 to tag2 -}
mergeTags :: ZoteroTagID -> ZoteroTagID -> DBConn ()
mergeTags oldTagID newTagID = do
  let oldID = fromIntToInt64 oldTagID
  let newID = fromIntToInt64 newTagID

  sqlRun sql1 [HDBC.SqlInt64 newID, HDBC.SqlInt64 oldID, HDBC.SqlInt64 newID]
  sqlRun sql2 [HDBC.SqlInt64 oldID]
  sqlRun sql3 [HDBC.SqlInt64 oldID]
  where
    -- Step 1 - rename tag from items with old tag but doesn't have the new tag
    sql1 = unlines [
                   "UPDATE itemTags"
                   ,"SET    tagID = ?"   -- (New tag)  set tag ID to 288 'cpp'
                   ,"WHERE  tagID = ?"  -- (Old tag) tag which name is 'c++'
                   ,"AND    itemID NOT IN (SELECT itemID FROM itemTags WHERE tagID = ?)"
                   ]
    -- Step 2 - delete all rows from itemTags which has the old tag.
    sql2 = "DELETE FROM itemTags WHERE tagID = ?"
    -- Step 3 - delete the old tag
    sql3 = "DELETE FROM tags WHERE tagID = ?"


createTag :: String -> DBConn Int
createTag tagName =  do
  key <- liftIO $  makeKey
  let sqlValues = [HDBC.SqlString tagName
                  ,HDBC.SqlString key
                  ,HDBC.SqlString tagName
                  ]
  sqlRun sql1 sqlValues
  fromJust <$> sqlQueryOne sql2 [HDBC.SqlString tagName] fromSqlToInt
  where
    sql2 = "SELECT tagID FROM tags WHERE name = ?"
    sql1 = unlines [
             "INSERT INTO tags (name, type, key)"
           ,"SELECT ?, 0, ?"
           -- Ensure that the tag is not inserted twice
           ,"WHERE NOT EXISTS (SELECT 1 FROM tags WHERE name = ?) ;"
           -- Return the tagID of the new tag inserted or existing
           ]


addTagNameToItem :: ZoteroItemID -> ZoteroTagName -> DBConn ()
addTagNameToItem itemID' tagName' = do
  let itemID = HDBC.SqlInt64 $ fromIntToInt64 itemID'
  let tagName = HDBC.SqlString tagName'
  tagID' <- createTag tagName'
  let tagID = HDBC.SqlInt64 $ fromIntToInt64 tagID'
  sqlRun sql [itemID, tagID, tagID, tagID, itemID]
  where
    sql = unlines [
                   "INSERT INTO itemTags (itemID, tagID)"
                  ,"SELECT ?, ?"
                  ,"WHERE EXISTS (SELECT 1 FROM tags WHERE tagID = ?)"
                  ,"AND NOT EXISTS (SELECT 1 FROM itemTags WHERE tagID = ? AND itemID = ?)"
                  ]

{- | Add multiple tags to an itemID -}          
addTagsToItem :: ZoteroItemID -> [ZoteroTagName] -> DBConn ()
addTagsToItem itemID tags = mapM_ (addTagNameToItem itemID) tags
                            
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
  key <- liftIO $ makeKey
  id <- fromJust <$> sqlQueryOne sql [HDBC.SqlString name, HDBC.SqlString key] fromSqlToInt
  return id 
  where
    sql = "WITH rows as (\
          \INSERT INTO collections (collectionID, collectionName, key)\
          \VALUES ( (SELECT 1 + max(collectionID) FROM collections), ?, ?)\
          \)\
          \SELECT max(collectionID) FROM collections"


insertItemID :: Int -> DBConn Int
insertItemID itemTypeID = do
  key    <- liftIO makeKey
  sqlRun sql1 [fromIntToHDBC itemTypeID, fromStrToHDBC key]
  itemID <- fromJust <$> sqlQueryOne sql2 [fromStrToHDBC key] fromSqlToInt
  return itemID
  where
    sql1 = "INSERT INTO items (itemTypeID, key) VALUES (?, ?)"
    sql2 = "SELECT itemID FROM items WHERE key = ?"

insertField :: Int -> (Int, String) -> DBConn ()
insertField itemID (fieldID, value) = do
  -- Insert value into table itemDataValues and get valueID
  sqlRun sql1 [fromStrToHDBC value]
  valueID <- sqlGetLastID "itemDataValues" "valueID"
  -- Insert into table itemData
  sqlRun sql2 [fromIntToHDBC itemID, fromIntToHDBC fieldID, fromIntToHDBC valueID]
  where
    sql1 = "INSERT INTO itemDataValues (value) VALUES (?)"
    sql2 = "INSERT INTO itemData VALUES (?, ?, ?)"



insertItem itemTypeID fieldList = do
  -- Insert item type into table items and get itemID
  key    <- liftIO makeKey
  sqlRun sql1 [fromIntToHDBC itemTypeID, fromStrToHDBC key]
  itemID <- sqlGetLastID "items" "itemID"
  mapM_ (insertField itemID) fieldList
  return itemID
      where
        sql1 = "INSERT INTO items (itemTypeID, key) VALUES (?, ?)"
        sql1 = "INSERT INTO items (itemTypeID, key) VALUES (?, ?)"


{- | Print Debug string if the environment variable DEBUG is set to true -}
printDebug :: String -> String -> DBConn ()
printDebug funName str = liftIO $ do
  dbg <- Env.lookupEnv "DEBUG"
  case dbg of
    Just "true" -> putStrLn $ P.printf "\nDEBUG (%s) = %s" funName str
    _           -> return ()


deleteItem :: Int -> DBConn ()
deleteItem itemID =  do
  -- sqlRun sqlDeleteDataValues [fromIntToHDBC itemID]
  sqlRun sqlDeleteWords [fromIntToHDBC itemID]
  sqlDeleteRowsWhereID "sourceItemID" "itemAttachments" itemID
  sqlDeleteRowsTablesWhereID "itemID" tables itemID
       where
         tables = [
                 "itemData"
                ,"itemTags"
                ,"itemCreators"
                ,"highlights"
                ,"itemNotes"
                ,"collectionItems"
                ,"itemSeeAlso"
                ,"itemAttachments"
                ,"items"
                ]
         sqlDeleteDataValues =  "DELETE FROM itemDataValues \
                                \WHERE valueID IN (SELECT valueID FROM itemData WHERE itemID = ?)"

         sqlDeleteWords = "DELETE FROM fullTextWords \
                          \WHERE wordID IN (SELECT wordID FROM fullTextItemWords WHERE itemID = ?)"

    -- sql7 = "DELETE FROM fullTextItems WHERE itemID = ?"
    -- sql8 = "DELETE FROM fullTextItemWords WHERE itemID = ?"



{- ============  Update Functions  ================ -}

getItemField :: ZoteroFieldID -> ZoteroItemID -> DBConn (Maybe ZoteroFieldValue)
getItemField fieldID itemID = sqlQueryOne sql [fromIntToHDBC fieldID, fromIntToHDBC itemID] fromSqlToString
     where
       sql = unlines [
        "SELECT  itemDataValues.value"
        ,"FROM    fields, itemDataValues, itemData"
        ,"WHERE   itemData.fieldID = fields.fieldID"
        ,"AND     itemData.valueID = itemDataValues.valueID"
        ,"AND     itemData.itemID = ?"
        ,"AND     itemData.fieldID = ?"
        ]

getItemURL :: ZoteroItemID -> DBConn (Maybe String)
getItemURL = getItemField 1

getItemISBN :: ZoteroItemID -> DBConn (Maybe String)
getItemISBN = getItemField 11

getItemISSN :: ZoteroItemID -> DBConn (Maybe String)
getItemISSN = getItemField 13

getItemDOI :: ZoteroItemID -> DBConn (Maybe String)
getItemDOI = getItemField 26

getItemTitle :: ZoteroItemID -> DBConn (Maybe String)
getItemTitle = getItemField 110


updateItemDataField :: Int -> String -> Int -> DBConn ()
updateItemDataField  fieldID value itemID = sqlRun sql [ fromStrToHDBC value
                                                        ,fromIntToHDBC itemID
                                                        ,fromIntToHDBC fieldID
                                                       ]
  where
    sql = unlines [
                   "UPDATE itemDataValues"
                  ,"SET value = ?"
                  ,"WHERE valueID = ("
                  ,"  SELECT valueID FROM itemData"
                  ,"  WHERE itemID = ?"
                  ,"  AND fieldID = ? )"
                 ]


{- | Update item title -}
updateItemTitle :: String -> Int -> DBConn ()
updateItemTitle  = updateItemDataField 110

{- | Update item publisher -}
updateItemPublisher :: String -> Int -> DBConn ()
updateItemPublisher = updateItemDataField 8

{- | Update item Date -}
updateItemDate :: String -> Int -> DBConn ()
updateItemDate = updateItemDataField 14

{- | Update item Abstract -}
updateItemAbstract :: String -> Int -> DBConn ()
updateItemAbstract  = updateItemDataField 90

updateItemDOI :: String -> Int -> DBConn ()
updateItemDOI = updateItemDataField 26

updateItemISBN :: String -> Int -> DBConn ()
updateItemISBN = updateItemDataField 11

updateItemISSN :: String -> Int -> DBConn ()
updateItemISSN  = updateItemDataField 13
