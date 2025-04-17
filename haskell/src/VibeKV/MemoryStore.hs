{-# LANGUAGE TypeFamilies #-}
module VibeKV.MemoryStore
  ( MemoryStore
  , newMemoryStore
  ) where

import VibeKV.Store
import qualified Data.Map.Strict as Map
import Data.IORef
import Data.Word (Word64)

-- | In-memory implementation of the Store interface
newtype MemoryStore = MemoryStore (IORef (Map.Map Word64 Word64))

-- | Create a new in-memory store
newMemoryStore :: IO MemoryStore
newMemoryStore = do
  ref <- newIORef Map.empty
  return $ MemoryStore ref

instance Store MemoryStore where
  -- | Put a key-value pair into the store
  put (MemoryStore ref) key value = do
    modifyIORef' ref (Map.insert key value)
    return $ MemoryStore ref

  -- | Get a value by key from the store
  get (MemoryStore ref) key = do
    map' <- readIORef ref
    return $ case Map.lookup key map' of
      Just value -> Right value
      Nothing -> Left KeyNotFound

  -- | Delete a key-value pair from the store
  delete (MemoryStore ref) key = do
    map' <- readIORef ref
    case Map.lookup key map' of
      Just _ -> do
        modifyIORef' ref (Map.delete key)
        return $ Right $ MemoryStore ref
      Nothing -> return $ Left KeyNotFound

  -- | Close the store and release any resources
  close _ = return ()
