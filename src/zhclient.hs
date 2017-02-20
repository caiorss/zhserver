{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}



{- |
Module      : Zhclient
Description : Command line client to access and test the database.
License     : Public Domain


Interface to Zotero database to query and manipulate the database.

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

import qualified System.Process as SP
import qualified System.FilePath as SF

{- | Apply a monadic function to a Maybe value if the value is not Nothing -}
iterMaybe :: Monad m => Maybe a -> (a -> m ()) -> m ()
iterMaybe value action = do
  case value of
    Just a   -> action a
    Nothing  -> return ()


iterMaybeError2 :: Monad m => Maybe a -> Maybe b -> m () -> (a -> b -> m ())  -> m ()
iterMaybeError2 a b errorHandler action  = do
  case (a, b) of
    (Just a', Just b') -> action a' b'
    _                  -> errorHandler


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

{- | Print all collections. -}
printCollections :: DBConn ()
printCollections  = do
  colls <- Z.getCollections
  liftIO $ mapM_ printColl colls
  where
    printColl coll =
      Text.Printf.printf "\t%d\t%s\n" (Z.zoteroCollID coll) (Z.zoteroCollName coll)

{- | Print all top level collections. Collections without any child collection -}
printCollectionsTop :: DBConn ()
printCollectionsTop  = do
  colls <- Z.getCollectionsTop
  liftIO $ mapM_ printColl colls
  where
    printColl coll =
      Text.Printf.printf "\t%d\t%s\n" (Z.zoteroCollID coll) (Z.zoteroCollName coll)

{- | Print all subcollections of a collection -}
printSubCollections :: Int -> DBConn ()
printSubCollections collID = do
  colls <- Z.getSubcollectionsIDNames collID
  liftIO $ mapM_ print colls

{- | Print all subcollections recursively of a given collection defined by its ID -}
printAllSubCollections :: Int -> DBConn ()
printAllSubCollections collID = do
  colls <- Z.getAllSubCollections collID
  liftIO $ mapM_ print colls

{- | Print all available tags and its ids -}
printTags :: DBConn ()
printTags = do
  tags    <- Z.getTags
  liftIO  $  mapM_ printTag tags
  where
    printTag tag =
      Text.Printf.printf "\t%d\t%s\n" (Z.zoteroTagID tag) (Z.zoteroTagName tag)

{- | Print all authors and its IDs -}
printAuthors :: DBConn ()
printAuthors = do
  authors <- Z.getAuthors
  liftIO  $ mapM_ printOne authors
  where
    printOne a =
      Text.Printf.printf "\t%d\t%s - \t\t%s\n" (Z.zoteroAuthorID a)
                                               (Z.zoteroAuthorLastName a)
                                               (Z.zoteroAuthorFirstName a)

{- | Print an item record -}
printItem :: Z.ZoteroItem -> DBConn ()
printItem item = do
  conn         <- ask
  let itemID   = Z.zoteroItemID   item
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

              printField  "\n\nAbstract :\n" "abstractNote"
              putStrLn ""
              mapM_ putStrLn path
              putStrLn "--------------------------------------------\n\n"

{- | Print an item given by its ID -}
printItemID :: Int -> DBConn ()
printItemID itemID = do
    item  <- Z.getZoteroItem itemID
    printItem item

{- | Open item file with default system application -}
openItem :: String -> Int -> DBConn ()
openItem storagePath itemID = do 
  item <-  Z.getZoteroItem itemID
  -- file :: Maybe String 
  let file =  Z.zoteroItemFile item
  -- path :: Maybe String 
  let filePath =  fmap (SF.combine storagePath) file     
  printItem item
  iterMaybe filePath  (\file -> liftIO $ xdgOpen file) 
  where
    -- Open file with default system application.
    xdgOpen :: String -> IO () 
    xdgOpen file = do
      _ <- SP.spawnProcess "xdg-open" [file]
      return ()
  


countItems :: [a] -> DBConn ()
countItems items = liftIO $ putStrLn $ "Count = " ++ show (length items)


prompt msg = do
  putStr  msg
  line    <- getLine
  return line 


{- | Print a collection given by its ID -}
printCollection :: Int -> DBConn ()
printCollection collID = do
  name  <- Z.getCollName collID
  items <- Z.collectionItems collID
  iterMaybe name (\a -> liftIO $ do putStrLn $ "Collection = " ++ a
                                    putStrLn "===============================\n\n"

                 )
  mapM_ printItemID items



printTagID :: Int -> DBConn ()
printTagID tagID = do
  name     <- Z.getTagName tagID
  related  <- Z.getRelatedTags tagID

  iterMaybe name (\a -> liftIO $ do putStrLn $ "Tag = " ++ a
                                    putStrLn "===============================\n\n"
                  )

  liftIO $ putStr "Realated tags: " >> mapM_ print related
  liftIO $ putStrLn "\n"  
  Z.getTagItems tagID >>= mapM_ printItemID

-- printSearchWordsTagsAnd [String] -> DBConn ()
-- printSearchWordsTagsAnd words =
--   searchByTitleTagsAndInWords words

{- | Parse Integer - Not a safe function -}
readInt :: String -> Int
readInt s = read s

wordLike s = "%" ++ s ++ "%"



copyCollectionTo :: String -> String -> Int -> DBConn ()
copyCollectionTo storagePath dest collID = do
  items       <- Z.collectionItems collID
  attachments <- mapM (\item -> Z.itemAttachmentFile item ) items
  liftIO $ createDirectoryIfMissing True dest
  liftIO $ mapM_ copyToDest attachments
  where
    destPath itemFile =
      SF.joinPath [dest, SF.takeFileName itemFile]

    copyToDest :: Maybe FilePath -> IO ()
    copyToDest itemFile =
      mapM_ (\file -> copyFile (SF.combine storagePath file) (destPath file)) itemFile

          
{- | Copy item file to a destination
- storagePath: Path where zotero's files are stored.
- dest:        Destination directory.
- itemID:      ID of item to be copied.
-}
copyItemTo :: String -> String -> Int -> DBConn ()
copyItemTo storagePath dest itemID = do
  itemFile <- Z.itemAttachmentFile itemID
  case itemFile of
    Nothing    -> liftIO $ putStrLn $ "Error: Item ID = " ++ show itemID ++ " has no associated file."
    Just file  -> do liftIO $ createDirectoryIfMissing True dest
                     let origin = SF.combine storagePath file
                     let output = SF.joinPath [dest, SF.takeFileName file]
                     liftIO $ copyFile origin output
                     liftIO $ putStrLn $ "Copied file '" ++ origin ++ "' to '" ++ output ++ "'\n\n"
                     printItemID itemID       

             
-- @HERE
parseArgs :: [String] -> String -> DBConn ()
parseArgs args path = do
  conn <- ask
  case args of

    -- ============ Items command line switches ========================
    -- 
    ["item", "-show",  itemID]                     -> printItemID (read itemID :: Int)
    ["item", "-open", itemID]                      -> openItem path (readInt itemID)
    ["item", "-copy", itemID, destPath]            -> copyItemTo path destPath (readInt itemID)                                                      
    ["item", "-delete", itemID]                    -> undefined
    
    ["item", "-add-tag", itemID,  tagNames] -> do Z.addTagsToItem (readInt itemID) (words tagNames)
                                                  printItemID (readInt itemID)
                                                             
    -- ["item", "-add-tag", itemID, "-tag-id", tagID]     -> Z.addTagToItem (readInt itemID) (readInt tagID)

    -- ============ Collections command line switches ====================
    -- 
    ["coll", "-show",  collID]                    -> printCollection (read collID :: Int)
    ["coll", "-all"]                               -> printCollections
    ["coll", "-top"]                               -> printCollectionsTop
    ["coll", "-count", collID]                     -> Z.collectionItems (readInt collID) >>= countItems
    ["coll", "-search", name]                      -> Z.searchCollection (wordLike name) >>= mapM_ (\ tpl -> liftIO $ print tpl)
    ["coll", "-copy-to", collID, destPath]         -> copyCollectionTo path destPath (readInt collID)
                                                       

    ["subcoll", collID]                            -> printSubCollections (read collID :: Int)
    ["subcoll", "-all",   collID]                  -> printAllSubCollections (read collID :: Int)
    ["subcoll", "-items", collID]                  -> Z.getAllSubCollectionsItems (read collID :: Int) >>= mapM_ printItemID

    -- ============= Tags command line switches ===========================
    --
    ["tag",  "-all"]                               -> printTags
    ["tag",  "-show", tagID]                      -> printTagID (readInt tagID)
    ["tag",  "-count", tagID]                      -> Z.getTagItems (readInt tagID) >>= countItems
    ["tag",  "-rename", tagID, newName]            -> Z.renameTag   (readInt tagID) newName
    ["tag",  "-delete", tagID]                     -> undefined
    ["tag",  "-merge", oldTagID, newTagID]         -> Z.mergeTags (readInt oldTagID) (readInt newTagID)
    ["tag", "-search", name]                       -> Z.searchTag (wordLike name) >>= mapM_ (\ tpl -> liftIO $ print tpl)

    -- ============  Author command line switches =======================
    --
    ["author", "-all"]                             -> printAuthors
    ["author", "-id", authorID]                    -> undefined
    ["author", "-search", name]                    -> undefined 
    ["author", "-show", authorID]                  -> Z.getItemsFromAuthor (read authorID :: Int) >>= mapM_ printItemID

    -- ============ Search commands ======================================
    --
    ["search", "-title-tag", word]                 -> Z.searchByTitleTags word >>= mapM_ printItemID
    ["search", "-title", word]                     -> Z.searchByTitleWordLike (wordLike word) >>= mapM_ printItemID
    ["search", "-title-content", word]             -> Z.searchByContentAndTitleLike word >>= mapM_ printItemID
    
    ["search", "-title-tag-and", query]            -> Z.searchByTitleTagsAndInWords (words query) >>= mapM_ printItemID
    ["search", "-title-tag-or", query]             -> Z.searchByTitleTagsOrInWords  (words query) >>= mapM_ printItemID

    
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
