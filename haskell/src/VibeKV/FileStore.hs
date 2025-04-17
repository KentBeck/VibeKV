{-# LANGUAGE TypeFamilies #-}
module VibeKV.FileStore
  ( FileStore
  , newFileStore
  ) where

import VibeKV.Store
import qualified Data.Map.Strict as Map
import Data.IORef
import System.IO
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import Control.Exception (bracket)
import System.Directory (doesFileExist)

-- | Operation types for the transaction log
data Operation = OpPut Word64 Word64 | OpDelete Word64
  deriving (Show, Eq)

-- | File-based implementation of the Store interface
data FileStore = FileStore
  { fsData     :: IORef (Map.Map Word64 Word64) -- In-memory cache of the current state
  , fsFile     :: Handle                        -- File handle for the transaction log
  , fsFilePath :: FilePath                      -- Path to the transaction log file
  }

-- | Create a new file-based store
newFileStore :: FilePath -> IO FileStore
newFileStore filePath = do
  -- Open the file for reading and writing, create if it doesn't exist
  fileExists <- doesFileExist filePath
  handle <- openBinaryFile filePath ReadWriteMode
  
  -- Create the store
  dataRef <- newIORef Map.empty
  let store = FileStore dataRef handle filePath
  
  -- Recover state from the transaction log if the file exists
  when fileExists $ recoverState store
  
  -- Seek to the end of the file for appending new records
  hSeek handle SeekFromEnd 0
  
  return store

-- | Recover state from the transaction log
recoverState :: FileStore -> IO ()
recoverState store = do
  -- Seek to the beginning of the file
  hSeek (fsFile store) AbsoluteSeek 0
  
  -- Read the entire file
  contents <- BL.hGetContents (fsFile store)
  
  -- Process each record
  let operations = parseOperations contents
  
  -- Apply operations to the in-memory state
  forM_ operations $ \op -> case op of
    OpPut key value -> modifyIORef' (fsData store) (Map.insert key value)
    OpDelete key -> modifyIORef' (fsData store) (Map.delete key)

-- | Parse operations from the transaction log
parseOperations :: BL.ByteString -> [Operation]
parseOperations bs
  | BL.null bs = []
  | otherwise = case runGetOrFail getOperation bs of
      Left _ -> []  -- Error parsing, stop
      Right (rest, _, op) -> op : parseOperations rest

-- | Parse a single operation from the transaction log
getOperation :: Get Operation
getOperation = do
  opType <- getWord8
  key <- getWord64be
  case opType of
    1 -> do  -- OpPut
      value <- getWord64be
      return $ OpPut key value
    2 -> return $ OpDelete key  -- OpDelete
    _ -> fail $ "Unknown operation type: " ++ show opType

-- | Append an operation to the transaction log
appendOperation :: FileStore -> Operation -> IO ()
appendOperation store op = do
  let bs = runPut $ putOperation op
  BL.hPut (fsFile store) bs
  hFlush (fsFile store)

-- | Serialize an operation to the transaction log
putOperation :: Operation -> Put
putOperation (OpPut key value) = do
  putWord8 1  -- OpPut
  putWord64be key
  putWord64be value
putOperation (OpDelete key) = do
  putWord8 2  -- OpDelete
  putWord64be key

instance Store FileStore where
  -- | Put a key-value pair into the store
  put store key value = do
    -- Append to the transaction log first
    appendOperation store (OpPut key value)
    
    -- Update in-memory state
    modifyIORef' (fsData store) (Map.insert key value)
    
    return store
  
  -- | Get a value by key from the store
  get store key = do
    map' <- readIORef (fsData store)
    return $ case Map.lookup key map' of
      Just value -> Right value
      Nothing -> Left KeyNotFound
  
  -- | Delete a key-value pair from the store
  delete store key = do
    map' <- readIORef (fsData store)
    case Map.lookup key map' of
      Just _ -> do
        -- Append to the transaction log first
        appendOperation store (OpDelete key)
        
        -- Update in-memory state
        modifyIORef' (fsData store) (Map.delete key)
        
        return $ Right store
      Nothing -> return $ Left KeyNotFound
  
  -- | Close the store and release any resources
  close store = hClose (fsFile store)

-- Utility functions
when :: Bool -> IO () -> IO ()
when True action = action
when False _ = return ()

forM_ :: [a] -> (a -> IO ()) -> IO ()
forM_ xs f = mapM_ f xs
