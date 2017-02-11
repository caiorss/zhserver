#!/usr/bin/stack runhaskell

-- import qualified System.Process as P

import Control.Monad (mapM_, forM_)
import System.Process
import qualified System.Directory as D
import GHC.IO.Exception (ExitCode)


getExitStatus (s, _, _) = s
getStdOut     (_, o, _) = o
getStdErr     (_, _, e) = e


runProcStdin cmd args stdin =
  readCreateProcessWithExitCode (proc cmd args) stdin 

runProc cmd args =
  runProcStdin cmd args ""

-- Run a command getting only stdout 
runStrings cmd args =
  getStdOut <$> runProcStdin cmd args ""

-- Run a command and print the stdout output 
runShowOut cmd args =
  putStrLn =<< getStdOut <$> runProcStdin cmd args ""

runShow cmd args stdin = do 
  (st, out, err) <- runProcStdin cmd args stdin
  if out /= "" then putStrLn out else return ()
  if err /= "" then putStrLn err else return ()
  putStrLn $ "  Exit Status = " ++ show st 


  
runShowDbg cmd args stdin = do 
  (st, out, err) <- runProcStdin cmd args stdin
  putStrLn $ "Running $ " ++ cmd ++ unwords args
  putStrLn $ "  Exit Status = " ++ show st 
  if out /= "" then putStrLn out else return ()
  if err /= "" then putStrLn err else return ()

  
 
-- Run a command and return its stdout lines 
runLines cmd args stdin =
  lines . getStdOut <$> readCreateProcessWithExitCode (proc cmd args) stdin 


{- --------------------------------------}

quote s = "'" ++ s ++ "'"

runPsqlDb database query  = runShowDbg "psql" args ""
  where
    args = ["-h", "localhost", "-U", "postgres", database, "-c", query]

runPsql query  = runShowDbg "psql" args ""
  where
    args = ["-h", "localhost", "-U", "postgres", "-c", query ]


runPsqlFile sqlFile database = do
  sql <- readFile sqlFile 
  runProcStdin "psql" args sql
  where
    args  = ["-h", "localhost", "-U", "postgres", database ]

putLine = putStrLn "----------------------------------------"

table2CsvFile table = "tables/" ++ table ++ ".csv"

              
tableToCsv :: String -> String -> IO ()  
tableToCsv dbFile table = do
  putStrLn $ "Exporting table " ++ table ++ " to " ++ csvfile  
  runShowDbg "sqlite3" [dbFile] stdin
  putLine 
  where
    csvfile  = table2CsvFile table 
    stdin = unlines [
                     ".headers on "
                    ,".mode csv"
                    ,".output " ++ csvfile
                    ,"\nSELECT * FROM " ++ table ++ " ;"
                   ]
  
copyTable :: String -> IO ()
copyTable table = do
  putStrLn $ "Copying table " ++ table 
  let csvfile = table2CsvFile table
  runPsqlDb "zotero" $ "\\copy " ++ table
            ++ " FROM " ++ csvfile
            ++ " DELIMITER ',' CSV HEADER "                           

getSqliteTables dbFile =
  runLines "sqlite3" [dbFile,
                      "SELECT name FROM sqlite_master WHERE type='table';"] ""

main = do
  let dbFile = "zotero.sqlite"

  --- Delete database 
  runPsql "DROP DATABASE zotero"

  -- Create database 
  runPsql "CREATE DATABASE zotero"

  --- cat tables.pgsql | psql -h localhost -U postgres zotero | tee error.log

  runPsqlFile "tables.pgsql" "zotero"

  --- Create tables 
  
  tables <- getSqliteTables "zotero.sqlite"
  forM_ tables (\tab -> do
                           tableToCsv dbFile tab 
                           copyTable  tab
                           D.removeFile $ table2CsvFile tab 
               ) 


