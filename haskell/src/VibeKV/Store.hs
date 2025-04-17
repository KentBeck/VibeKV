{-# LANGUAGE TypeFamilies #-}
module VibeKV.Store
  ( Store(..)
  , KeyNotFound(..)
  ) where

-- | Exception thrown when a key is not found
data KeyNotFound = KeyNotFound
  deriving (Show, Eq)

-- | Store interface for key-value stores
class Store s where
  -- | Put a key-value pair into the store
  put :: s -> Word64 -> Word64 -> IO s
  
  -- | Get a value by key from the store
  get :: s -> Word64 -> IO (Either KeyNotFound Word64)
  
  -- | Delete a key-value pair from the store
  delete :: s -> Word64 -> IO (Either KeyNotFound s)
  
  -- | Close the store and release any resources
  close :: s -> IO ()
