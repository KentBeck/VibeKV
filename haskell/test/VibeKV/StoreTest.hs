module VibeKV.StoreTest
  ( tests
  , testPut
  , testGet
  , testDelete
  , testDeleteNonExistentKey
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import VibeKV.Store

tests :: TestTree
tests = testGroup "Store Interface Tests" []

-- | Test the Put operation
testPut :: (Store s) => String -> s -> (s -> IO ()) -> TestTree
testPut name store cleanup = testCase (name ++ " - Put") $ do
  let key = 42
      value = 123
  
  -- Put a key-value pair
  store' <- put store key value
  
  -- Cleanup
  cleanup store'

-- | Test the Get operation
testGet :: (Store s) => String -> s -> (s -> IO ()) -> TestTree
testGet name store cleanup = testCase (name ++ " - Get") $ do
  let key = 42
      value = 123
  
  -- Put a key-value pair
  store' <- put store key value
  
  -- Get the value
  result <- get store' key
  case result of
    Right retrievedValue -> 
      assertEqual "Retrieved value should match original" value retrievedValue
    Left KeyNotFound -> 
      assertFailure "Key should exist"
  
  -- Try to get a non-existent key
  let nonExistentKey = 999
  result' <- get store' nonExistentKey
  case result' of
    Right _ -> 
      assertFailure "Non-existent key should not be found"
    Left KeyNotFound -> 
      return ()
  
  -- Cleanup
  cleanup store'

-- | Test the Delete operation
testDelete :: (Store s) => String -> s -> (s -> IO ()) -> TestTree
testDelete name store cleanup = testCase (name ++ " - Delete") $ do
  let key = 42
      value = 123
  
  -- Put a key-value pair
  store' <- put store key value
  
  -- Delete the key
  result <- delete store' key
  case result of
    Right store'' -> do
      -- Try to get the deleted key
      result' <- get store'' key
      case result' of
        Right _ -> 
          assertFailure "Deleted key should not be found"
        Left KeyNotFound -> 
          return ()
      
      -- Cleanup
      cleanup store''
    
    Left KeyNotFound -> do
      assertFailure "Key should exist for deletion"
      cleanup store'

-- | Test deleting a non-existent key
testDeleteNonExistentKey :: (Store s) => String -> s -> (s -> IO ()) -> TestTree
testDeleteNonExistentKey name store cleanup = testCase (name ++ " - Delete Non-Existent Key") $ do
  -- Try to delete a non-existent key
  let nonExistentKey = 999
  result <- delete store nonExistentKey
  case result of
    Right _ -> 
      assertFailure "Non-existent key should not be deleted"
    Left KeyNotFound -> 
      return ()
  
  -- Cleanup
  cleanup store
