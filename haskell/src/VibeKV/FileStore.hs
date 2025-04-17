{-# LANGUAGE TypeFamilies #-}
module VibeKV.FileStore
  ( FileStore
  , newFileStore
  ) where

import VibeKV.Store
import qualified Data.Map.Strict as Map
import Data.IORef

import Data.Bits (shiftL, shiftR, (.|.))
import qualified Data.ByteString as BS
import System.Directory (doesFileExist)
import Data.Word (Word64)

-- | Operation types for the transaction log
data Operation = OpPut Word64 Word64 | OpDelete Word64
  deriving (Show, Eq)

-- | File-based implementation of the Store interface
data FileStore = FileStore
  { fsData     :: IORef (Map.Map Word64 Word64) -- In-memory cache of the current state
  , fsFilePath :: FilePath                      -- Path to the transaction log file
  }

-- | Create a new file-based store
newFileStore :: FilePath -> IO FileStore
newFileStore filePath = do
  -- Create the store
  dataRef <- newIORef Map.empty
  let store = FileStore dataRef filePath

  -- Recover state from the transaction log if it exists
  fileExists <- doesFileExist filePath
  when fileExists $ recoverState store

  return store

-- | Recover state from the transaction log
recoverState :: FileStore -> IO ()
recoverState store = do
  -- Read the file content
  content <- BS.readFile (fsFilePath store)

  -- Parse operations and apply them
  let operations = parseOperations content
  forM_ operations $ \op -> case op of
    OpPut key value -> modifyIORef' (fsData store) (Map.insert key value)
    OpDelete key -> modifyIORef' (fsData store) (Map.delete key)

-- | Parse operations from the transaction log
parseOperations :: BS.ByteString -> [Operation]
parseOperations bs
  | BS.null bs = []
  | otherwise = case decodeOperation bs of
      Nothing -> []  -- Error parsing, stop
      Just (op, rest) -> op : parseOperations rest

-- | Decode a single operation from the transaction log
decodeOperation :: BS.ByteString -> Maybe (Operation, BS.ByteString)
decodeOperation bs
  | BS.length bs < 1 = Nothing
  | otherwise =
      let opType = BS.index bs 0
          bs' = BS.drop 1 bs
      in if BS.length bs' < 8
         then Nothing
         else
           let key = decodeWord64 (BS.take 8 bs')
               bs'' = BS.drop 8 bs'
           in case opType of
                1 -> if BS.length bs'' < 8
                     then Nothing
                     else Just (OpPut key (decodeWord64 (BS.take 8 bs'')), BS.drop 8 bs'')
                2 -> Just (OpDelete key, bs'')
                _ -> Nothing

-- | Decode a Word64 from a ByteString
decodeWord64 :: BS.ByteString -> Word64
decodeWord64 bs =
  let w1 = fromIntegral (BS.index bs 0) :: Word64
      w2 = fromIntegral (BS.index bs 1) :: Word64
      w3 = fromIntegral (BS.index bs 2) :: Word64
      w4 = fromIntegral (BS.index bs 3) :: Word64
      w5 = fromIntegral (BS.index bs 4) :: Word64
      w6 = fromIntegral (BS.index bs 5) :: Word64
      w7 = fromIntegral (BS.index bs 6) :: Word64
      w8 = fromIntegral (BS.index bs 7) :: Word64
  in (w1 `shiftL` 56) .|. (w2 `shiftL` 48) .|. (w3 `shiftL` 40) .|. (w4 `shiftL` 32) .|.
     (w5 `shiftL` 24) .|. (w6 `shiftL` 16) .|. (w7 `shiftL` 8) .|. w8

-- | Encode a Word64 to a ByteString
encodeWord64 :: Word64 -> BS.ByteString
encodeWord64 w = BS.pack
  [ fromIntegral (w `shiftR` 56)
  , fromIntegral (w `shiftR` 48)
  , fromIntegral (w `shiftR` 40)
  , fromIntegral (w `shiftR` 32)
  , fromIntegral (w `shiftR` 24)
  , fromIntegral (w `shiftR` 16)
  , fromIntegral (w `shiftR` 8)
  , fromIntegral w
  ]

-- | Append an operation to the transaction log
appendOperation :: FileStore -> Operation -> IO ()
appendOperation store op = do
  let bs = encodeOperation op
  -- Append to the file
  BS.appendFile (fsFilePath store) bs

-- | Encode an operation to a ByteString
encodeOperation :: Operation -> BS.ByteString
encodeOperation (OpPut key value) =
  BS.concat [BS.singleton 1, encodeWord64 key, encodeWord64 value]
encodeOperation (OpDelete key) =
  BS.concat [BS.singleton 2, encodeWord64 key]

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
  close _ = return ()

-- Utility functions
when :: Bool -> IO () -> IO ()
when True action = action
when False _ = return ()

forM_ :: [a] -> (a -> IO ()) -> IO ()
forM_ xs f = mapM_ f xs
