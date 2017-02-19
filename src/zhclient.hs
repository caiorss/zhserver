{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

{-

Description: Command line interactive client to access the database.  
File:        zhclient.hs 

-}

-- let destPath itemFile = SF.joinPath ["/tmp/test", SF.takeFileName itemFile]    

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader

import Text.Read (readMaybe)
import qualified System.Process as P
import qualified System.FilePath as SF
import System.Exit  (exitSuccess)

import qualified Zotero as Z
import Zotero (DBConn, joinStrings, strip)

import System.Directory (copyFile, createDirectoryIfMissing)

import Text.Printf (printf)

import qualified System.Environment as Env


copyCollectionTo conn collID dest = do
  items       <- runReaderT (Z.collectionItems collID) conn
  attachments <- mapM (\item -> runReaderT (Z.itemAttachmentFile item) conn) items
  createDirectoryIfMissing True dest
  mapM_ copyToDest attachments
  where
    destPath itemFile =
      SF.joinPath [dest, SF.takeFileName itemFile]

    copyToDest :: Maybe FilePath -> IO ()
    copyToDest itemFile =
      mapM_ (\i -> copyFile i (destPath i)) itemFile  



{- Last name comes first than first name in 
   order to make easier to locate the author.
-}   
printItemAuthor conn itemID = do
  authorData <- runReaderT (Z.itemAuthors itemID) conn
  mapM_ printRow authorData
  where
    printRow author = printf "%s: %s, %s\n" (Z.zoteroAuthorFirstName author)
                                            (Z.zoteroAuthorLastName  author)
                                            (show $ Z.zoteroAuthorID author)
printCollections :: DBConn ()
printCollections  = do
  colls <- Z.getCollections
  liftIO $ mapM_ printColl colls
  where
    printColl coll =
      Text.Printf.printf "\t%d\t%s\n" (Z.zoteroCollID coll) (Z.zoteroCollName coll)

printCollectionsTop :: DBConn ()
printCollectionsTop  = do
  colls <- Z.getCollectionsTop
  liftIO $ mapM_ printColl colls
  where
    printColl coll =
      Text.Printf.printf "\t%d\t%s\n" (Z.zoteroCollID coll) (Z.zoteroCollName coll)

printSubCollections :: Int -> DBConn ()
printSubCollections collID = do
  colls <- Z.getSubcollectionsIDNames collID
  liftIO $ mapM_ print colls


printAllSubCollections :: Int -> DBConn ()
printAllSubCollections collID = do
  colls <- Z.getAllSubCollections collID
  liftIO $ mapM_ print colls


printTags :: DBConn ()
printTags = do
  tags    <- Z.getTags
  liftIO  $  mapM_ printTag tags
  where
    printTag tag =
      Text.Printf.printf "\t%d\t%s\n" (Z.zoteroTagID tag) (Z.zoteroTagName tag)


printAuthors :: DBConn ()
printAuthors = do
  authors <- Z.getAuthors
  liftIO  $ mapM_ printOne authors
  where
    printOne a =
      Text.Printf.printf "\t%d\t%s - \t\t%s\n" (Z.zoteroAuthorID a)
                                               (Z.zoteroAuthorLastName a)
                                               (Z.zoteroAuthorFirstName a)


printItem :: Int -> DBConn ()
printItem itemID = do
  conn         <- ask
  item         <- Z.getZoteroItem itemID
  let itemdata = Z.zoteroItemData item
  let path     = Z.zoteroItemFile item
  let tags     = Z.zoteroItemTags item
  let colls    = Z.zoteroItemCollections item

  let printField label field = do
        mapM_ (\value -> do
                         putStr label
                         putStr value
                         putStr "\n"
               )
              (lookup field itemdata)


  liftIO $ do printField  "Title: "     "title"
              putStrLn    ""
              putStrLn $  "Item ID:     " ++ show itemID
              putStrLn $  "Tags:        " ++ joinStrings ", " (map show tags)
              putStrLn $  "Collections: " ++ joinStrings ", " (map show colls)
              printField  "Publisher:   " "publisher"
              printField  "Book Title:  " "bookTitle"
              printField  "Url:         " "url"
              printField  "DOI:         " "DOI"

              printItemAuthor conn itemID

              printField  "Abstract :\n" "abstractNote"
              putStrLn ""
              mapM_ putStrLn path
              putStrLn "--------------------------------------------\n\n"


countItems :: [a] -> DBConn ()
countItems items = liftIO $ putStrLn $ "Count = " ++ show (length items)


prompt msg = do
  putStr  msg
  line    <- getLine
  return line 


iterMaybe :: Monad m => Maybe a -> (a -> m ()) -> m ()
iterMaybe value action = do
  case value of
    Just a   -> action a
    Nothing  -> return ()

printCollection :: Int -> DBConn ()
printCollection collID = do
  name  <- Z.getCollName collID
  items <- Z.collectionItems collID
  iterMaybe name (\a -> liftIO $ do putStrLn $ "Collection = " ++ a
                                    putStrLn "===============================\n\n"

                 )
  mapM_ printItem items

-- printSearchWordsTagsAnd [String] -> DBConn ()
-- printSearchWordsTagsAnd words =
--   searchByTitleTagsAndInWords words

{- | Parse Integer - Not a safe function -}
readInt :: String -> Int
readInt s = read s
        
            
-- @HERE
parseArgs :: [String] -> String -> DBConn ()
parseArgs args path = do
  conn <- ask
  case args of

    -- ============ Items command line switches ========================
    -- 
    ["item", "-id",  itemID]                       -> printItem (read itemID :: Int)
    ["item", "-open", itemID]                      -> undefined 
    ["item", "-delete", itemID]                    -> undefined
    ["item", "-add-tag", itemID, "-tag-name", tagName] -> undefined 
    ["item", "-add-tag", itemID, "-tag-id", tagID]     -> Z.addTagToItem (readInt itemID) (readInt tagID)

    -- ============ Collections command line switches ====================
    -- 
    ["coll", "-id",  collID]                       -> printCollection (read collID :: Int)
    ["coll", "-all"]                               -> printCollections
    ["coll", "-top"]                               -> printCollectionsTop

    ["subcoll", collID]                            -> printSubCollections (read collID :: Int)
    ["subcoll", "-all",   collID]                  -> printAllSubCollections (read collID :: Int)
    ["subcoll", "-items", collID]                  -> Z.getAllSubCollectionsItems (read collID :: Int) >>= mapM_ printItem

    -- ============= Tags command line switches ===========================
    --
    ["tag",  "-all"]                               -> printTags
    ["tag",  "-items", tagID]                      -> Z.getTagItems (readInt tagID) >>= mapM_ printItem
    ["tag",  "-count", tagID]                      -> Z.getTagItems (readInt tagID) >>= countItems
    ["tag",  "-rename", tagID, newName]            -> Z.renameTag   (readInt tagID) newName
    ["tag",  "-delete", tagID]                     -> undefined
    ["tag",  "-merge", oldTagID, newTagID]         -> Z.mergeTags (readInt oldTagID) (readInt newTagID)

    -- ============  Author command line switches =======================
    --
    ["author", "-all"]                             -> printAuthors
    ["author", "-id", authorID]                    -> undefined
    ["author", "-search", name]                    -> undefined 
    ["author", "-items", authorID]                 -> Z.getItemsFromAuthor (read authorID :: Int) >>= mapM_ printItem 

    -- ============ Search commands ======================================
    --
    ["search", "-title-tag", word]                 -> Z.searchByTitleTags word >>= mapM_ printItem
    ["search", "-title", word]                     -> Z.searchByTitleWordLike ("%" ++ word ++ "%") >>= mapM_ printItem
    ["search", "-title-content", word]             -> Z.searchByContentAndTitleLike word >>= mapM_ printItem
    
    ["search", "-title-tag-and", query]            -> Z.searchByTitleTagsAndInWords (words query) >>= mapM_ printItem
    ["search", "-title-tag-or", query]             -> Z.searchByTitleTagsOrInWords  (words query) >>= mapM_ printItem

    
    []                                             -> liftIO $ putStrLn "Show help"
    _                                              -> liftIO $ putStrLn "Error: Invalid command."


main :: IO ()
main = do
  dbUri  <- Env.lookupEnv "ZOTERO_DB"
  pathSt   <- Env.lookupEnv "ZOTERO_PATH"
  case (dbUri, pathSt) of

       (Just db, Just path)   -> do args <- Env.getArgs
                                    Just dbcon <- Z.openDBConnection db
                                    Z.runDBConn dbcon (parseArgs args path)

       _                      -> do putStrLn "Error: I can't open the database connection."
                                    putStrLn "Set the environment variable ZOTERO_DB."
