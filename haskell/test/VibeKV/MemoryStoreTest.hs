module VibeKV.MemoryStoreTest
  ( tests
  ) where

import Test.Tasty
import VibeKV.Store
import VibeKV.MemoryStore
import qualified VibeKV.StoreTest as StoreTest
import System.IO.Unsafe (unsafePerformIO)

tests :: TestTree
tests = testGroup "MemoryStore Tests"
  [ testPut
  , testGet
  , testDelete
  , testDeleteNonExistentKey
  ]

-- | Test the Put operation for MemoryStore
testPut :: TestTree
testPut = withMemoryStore $ \store ->
  StoreTest.testPut "MemoryStore" store close

-- | Test the Get operation for MemoryStore
testGet :: TestTree
testGet = withMemoryStore $ \store ->
  StoreTest.testGet "MemoryStore" store close

-- | Test the Delete operation for MemoryStore
testDelete :: TestTree
testDelete = withMemoryStore $ \store ->
  StoreTest.testDelete "MemoryStore" store close

-- | Test deleting a non-existent key for MemoryStore
testDeleteNonExistentKey :: TestTree
testDeleteNonExistentKey = withMemoryStore $ \store ->
  StoreTest.testDeleteNonExistentKey "MemoryStore" store close

-- | Helper function to create a MemoryStore for testing
withMemoryStore :: (MemoryStore -> TestTree) -> TestTree
withMemoryStore f = unsafePerformIO $ do
  store <- newMemoryStore
  return (f store)


