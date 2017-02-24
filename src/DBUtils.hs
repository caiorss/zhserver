{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}


{- |
Module      : DBUtils 
Description : Database Utilities 
License     : Public Domain

High level functions for haskell HDBC.

-}

module DBUtils
    (
     -- * Types 
      SQL
     ,HDBConn (..)
     ,DBUriPath
     ,SearchIDFun
     ,SearchNameIdFun
     ,SQLQuery
     ,DBConn
     ,DBUri
          

     -- * Functions
     ,openDBConnection
     ,runDBConn
     ,withDBConnection
     ,withDBConnection2
     ,withDBConnection3
     ,withConnection
     ,makeSearchIdFun
     ,makeSearchNameIdFun

     ,fromInt64ToInt
     ,fromIntToInt64
     ,fromSqlToInt
     ,fromSqlToString

      ,fromIntToHDBC
      ,fromStrToHDBC
      
     ,sqlQuery
     ,sqlQueryAll
     ,sqlQueryRow
     ,sqlQueryOne
     ,sqlRun 

     ,joinStrings
     ,lookupString
     ,lookupInt
     ,sqlGetLastID
     
    ) where 

{--------------- Imports -----------------------}

import qualified Database.HDBC.Sqlite3 as Sqlite3
import qualified Database.HDBC.PostgreSQL as Pg
import qualified Database.HDBC as HDBC
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import qualified Data.Text as T 
import qualified Text.Printf as Printf
import Data.Int (Int64)
import Data.Maybe (fromJust)
    
{---------------- Types -------------------------}

{- | SQL Statement or SQL expression. -}
type SQL = String

{- | Database Uri string.  -}    
type DBUriPath = String 

{- | This type represents a function that takes name and returns all IDs
   associated with table rows which match the name
-}
type SearchIDFun = String -> DBConn [Int]


type SearchNameIdFun = String -> DBConn [(Int, String)]


type SQLQuery =  [(String, HDBC.SqlValue)] 

{- | Database Connection -> DbConn a = ReaderT conn IO a = conn -> IO a -}
type DBConn a = forall conn. (HDBC.IConnection conn) =>  ReaderT conn IO a

{- | Database Connection - Objective make the database connection
    implementation agnostic.
-}
data HDBConn =  HDBConnSqlite   Sqlite3.Connection
              | HDBConnPostgres Pg.Connection
              -- deriving (Eq, Read, Show)



{- | Database URI -}
data DBUri = DBUriSqlite   String
           | DBUriPostGres String
           deriving (Eq, Read, Show)


                    
stripPrefix prefix str =
 case T.stripPrefix (T.pack prefix) (T.pack str) of
   Just s   -> T.unpack s
   Nothing  -> str

parseDbDriver2 dbUri =
  case getDbType dbUri of
    "sqlite"    -> Just (DBUriSqlite   sqlitePath)
    "postgres"  -> Just (DBUriPostGres dbUri)
    _           -> Nothing
  where
    sqlitePath = (stripPrefix "sqlite://" dbUri)
    getDbType dbUri = T.unpack . (!!0) . T.split (==':') . T.pack $ dbUri


openDBConnection :: DBUriPath -> IO (Maybe HDBConn)
openDBConnection dbUri =
  case parseDbDriver2 dbUri of
    Just (DBUriSqlite   uri) -> Sqlite3.connectSqlite3  uri
                                >>= \conn -> return $ Just (HDBConnSqlite conn)

    Just (DBUriPostGres uri) -> Pg.connectPostgreSQL    uri
                                >>= \conn -> return $ Just ( HDBConnPostgres conn)

    Nothing                  -> return Nothing


runDBConn :: HDBConn -> DBConn a -> IO a
runDBConn hdbconn dbAction =
  case hdbconn of
    HDBConnSqlite   c  -> do out <- runReaderT dbAction c
                             HDBC.commit c
                             return out

    HDBConnPostgres c  -> do out <- runReaderT dbAction c
                             HDBC.commit c
                             return out



-- withDBConnection :: forall conn. (HDBC.IConnection conn) => String -> (conn -> IO ()) -> IO ()
withDBConnection ::  DBUriPath -> DBConn () -> IO ()
withDBConnection dbUri dbAction = do
  conn <- openDBConnection dbUri
  case conn of
    Just (HDBConnSqlite   c)  -> do runReaderT dbAction c
                                    HDBC.commit c
                                    HDBC.disconnect c

    Just (HDBConnPostgres c)  -> do runReaderT dbAction c
                                    HDBC.commit c
                                    HDBC.disconnect c

    Nothing                   -> putStrLn "Error: I can't open the database connection"



withDBConnection2 ::  String -> DBConn a -> IO (Maybe a)
withDBConnection2 dbUri dbAction = do
  conn <- openDBConnection dbUri
  case conn of
    Just (HDBConnSqlite   c)  -> do out <- runReaderT dbAction c
                                    HDBC.disconnect c
                                    HDBC.commit c
                                    return (Just out)

    Just (HDBConnPostgres c)  -> do out <- runReaderT dbAction c
                                    HDBC.disconnect c
                                    HDBC.commit c
                                    return (Just out)

    Nothing                   -> return Nothing



withDBConnection3 ::  String -> DBConn a -> IO a
withDBConnection3 dbUri dbAction = do
  conn <- openDBConnection dbUri
  case conn of
    Just (HDBConnSqlite   c)  ->  HDBC.withTransaction c $ \ conn -> do
                                     out <- runReaderT dbAction c
                                     -- HDBC.commit c
                                     -- HDBC.disconnect c
                                     return out

    Just (HDBConnPostgres c)  -> do out <- runReaderT dbAction c
                                    HDBC.disconnect c
                                    HDBC.commit c
                                    return out

    Nothing                   -> error "Error: Invalid database URI."




withConnection :: HDBC.IConnection  conn => IO conn -> (conn -> IO r) -> IO r
withConnection ioConn function = do
  conn     <- ioConn
  result   <- function conn
  HDBC.disconnect conn
  return result


{- | Make  function which searches by string or name and returns IDs -}
makeSearchIdFun :: SQL -> SearchIDFun
makeSearchIdFun sql = \ search -> sqlQueryRow sql [HDBC.SqlString search] fromSqlToInt


{- | Make function which searches items from table which column matches a string -}
makeSearchNameIdFun :: SQL -> SearchNameIdFun
makeSearchNameIdFun sql = \ search -> sqlQueryAll sql [HDBC.SqlString search] projection
                          where
                            projection row =  (fromSqlToInt (row !! 0),  (fromSqlToString (row !! 1)))




splitOn delim text =  
   map T.unpack $ T.splitOn (T.pack delim) (T.pack text)

strip  = T.unpack . T.strip . T.pack

fromInt64ToInt :: Int64 -> Int
fromInt64ToInt = fromIntegral

fromIntToInt64 :: Int -> Int64
fromIntToInt64 = fromIntegral

{- | Convert number to HDBC value -}
fromIntToHDBC :: Int -> HDBC.SqlValue
fromIntToHDBC n = HDBC.SqlInt64 $ fromIntegral n                 

{- | Convert string to HDBC string -}                  
fromStrToHDBC s = HDBC.SqlString s                 
                  
                  
{- | Join strings by separator -}
joinStrings :: String -> [String] -> String
joinStrings sep strs =
  case strs of
    [] -> ""
    _  -> foldr1 (\x acc ->  x ++ sep ++  acc) strs

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


sqlQuery :: SQL -> [HDBC.SqlValue] -> ([HDBC.SqlValue] -> b) -> DBConn (Maybe b)
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

sqlQueryAll :: SQL -> [HDBC.SqlValue] -> ([HDBC.SqlValue] -> b) -> DBConn [b]     
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

sqlQueryRow :: SQL -> [HDBC.SqlValue] -> (HDBC.SqlValue -> b) -> DBConn [b]   
sqlQueryRow sql sqlvals coercion = do
  sqlQueryAll sql sqlvals (coercion . (!!0))


sqlQueryOne :: SQL -> [HDBC.SqlValue] -> (HDBC.SqlValue -> b) -> DBConn (Maybe b)
sqlQueryOne sql sqlvals projection = do
  conn   <- ask 
ql stmt   <- liftIO $  HDBC.prepare conn sql
  liftIO $ HDBC.execute stmt sqlvals
  row     <- liftIO $ HDBC.fetchRow stmt
  liftIO $ HDBC.commit conn 
  return $ (!!0) . (map projection) <$> row

{- | Run a SQL statement that returns no value -}   
sqlRun :: SQL -> [HDBC.SqlValue] -> DBConn ()
sqlRun sql sqlvals = do
  conn    <- ask 
  stmt    <- liftIO $ HDBC.prepare conn sql
  liftIO  $ HDBC.execute stmt sqlvals
  liftIO  $ HDBC.commit conn 
    

sqlGetLastID :: String -> String -> DBConn Int
sqlGetLastID table column = fromJust <$> sqlQueryOne sql [] fromSqlToInt
  where
    sql = Printf.printf "SELECT max(%s) FROM %S" column table 
