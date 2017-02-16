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

printTags :: DBConn ()
printTags = do
  tags    <- Z.getTags
  liftIO  $  mapM_ printTag tags
  where
    printTag tag =
      Text.Printf.printf "\t%d\t%s\n" (Z.zoteroTagID tag) (Z.zoteroTagName tag)

printItem :: Int -> DBConn ()
printItem itemID = do
  conn     <- ask
  itemdata <- Z.itemData itemID
  path     <- Z.itemAttachmentFile itemID
  tags     <- Z.itemTags itemID

  let printField label field = do
        mapM_ (\value -> do
                         putStr label
                         putStr value
                         putStr "\n"
               )
              (lookup field itemdata)


  liftIO $ do printField  "Title: "      "title"
              putStrLn ""
              putStrLn   ("Item ID: " ++ show itemID)
              printField  "Publisher: "  "publisher"
              printField  "Book Title: " "bookTitle"
              printField  "Url: "        "url"
              printField  "DOI: "        "DOI"

              printItemAuthor conn itemID

              printField  "Abstract :\n" "abstractNote"
              putStrLn ""
              mapM_ putStrLn path
              putStrLn $ "Tags: " ++ joinStrings ", " tags
              putStrLn "--------------------------------------------"



prompt msg = do
  putStr  msg
  line    <- getLine
  return line 

printCollection :: Int -> DBConn ()
printCollection collID = do
  items <- Z.collectionItems collID
  mapM_ printItem items

  

-- repl :: conn -> IO ()
repl conn = forever $ do

  -- conn <- dbConnection

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
    
    -- "1" -> showCollections conn
    
    "2" -> do
             input <- prompt "Enter a collection ID: "  
             mapM_ (printCollection conn) (readMaybe input :: Maybe Int)

    "3" -> do
             input <- prompt "Enter a item ID: "

             let itemID = readMaybe input :: Maybe Int 

             mapM_ (\fid -> printItem conn fid ) itemID

             path <- fmap join $ mapM
               (\itemID -> runReaderT (Z.itemAttachmentFile itemID) conn)
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
            
            

parseArgs :: [String] -> DBConn ()
parseArgs args = do
  conn <- ask
  case args of
    ["item", "-id",  itemID] -> liftIO $ printItem       conn (read itemID :: Int)

    ["coll", "-id",  collID] -> liftIO $ printCollection conn (read collID :: Int)
    ["coll", "-all"]         -> printCollections

    ["tag",  "-all"]         -> printTags  
    
    []                       -> liftIO $ putStrLn "Show help"
    _                        -> liftIO $ putStrLn "Error: Invalid command."

main :: IO ()
main = do
  dbUri  <- Env.lookupEnv "ZOTERO_DB"
  case dbUri of
    Nothing  -> do putStrLn "Error: I can't open the database connection."
                   putStrLn "Set the environment variable ZOTERO_DB."

    Just db   -> do args <- Env.getArgs
                    Z.withDBConnection db (parseArgs args)
