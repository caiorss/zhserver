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


copyCollectionTo conn collID dest = do
  
  items       <- collectionItems conn collID

  attachments <- mapM (itemAttachmentFile conn) items
  
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

  authorData <- itemAuthors conn itemID
  mapM_ printRow authorData

  where
    printRow row = printf "%s: %s, %s\n" (row !! 2) (row !! 1) (row !! 0)



       
printItem conn itemID = do
  
  itemdata <- itemData conn itemID

  path <- itemAttachmentFile conn itemID

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


prompt msg = do
  putStr  msg
  line    <- getLine
  return line 



printCollection conn collID = do
  items <- collectionItems conn collID
  mapM_ (printItem conn) items 
  


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
               (\itemID -> itemAttachmentFile conn itemID)
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
            
            
