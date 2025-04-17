module VibeKV.FileStoreTest
  ( tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import VibeKV.Store
import VibeKV.FileStore
import qualified VibeKV.StoreTest as StoreTest
import System.IO.Temp
import System.FilePath
import System.Directory

tests :: TestTree
tests = testGroup "FileStore Tests"
  [ testPut
  , testGet
  , testDelete
  , testDeleteNonExistentKey
  , testRecovery
  , testComplexRecovery
  ]

-- | Test the Put operation for FileStore
testPut :: TestTree
testPut = withTempFileStore $ \store cleanup ->
  StoreTest.testPut "FileStore" store cleanup

-- | Test the Get operation for FileStore
testGet :: TestTree
testGet = withTempFileStore $ \store cleanup ->
  StoreTest.testGet "FileStore" store cleanup

-- | Test the Delete operation for FileStore
testDelete :: TestTree
testDelete = withTempFileStore $ \store cleanup ->
  StoreTest.testDelete "FileStore" store cleanup

-- | Test deleting a non-existent key for FileStore
testDeleteNonExistentKey :: TestTree
testDeleteNonExistentKey = withTempFileStore $ \store cleanup ->
  StoreTest.testDeleteNonExistentKey "FileStore" store cleanup

-- | Test recovery from the transaction log
testRecovery :: TestTree
testRecovery = testCase "FileStore - Recovery" $ do
  withSystemTempDirectory "vibekv-test" $ \tempDir -> do
    let filePath = tempDir </> "vibekv.log"
    
    -- Create a new file store
    store1 <- newFileStore filePath
    
    -- Add some data
    let key = 42
        value = 123
    
    store1' <- put store1 key value
    
    -- Close the store
    close store1'
    
    -- Create a new store with the same file
    store2 <- newFileStore filePath
    
    -- Verify the data was recovered
    result <- get store2 key
    case result of
      Right retrievedValue -> 
        assertEqual "Retrieved value should match original" value retrievedValue
      Left KeyNotFound -> 
        assertFailure "Key should exist after recovery"
    
    -- Close the store
    close store2

-- | Test recovery with multiple operations including deletions
testComplexRecovery :: TestTree
testComplexRecovery = testCase "FileStore - Complex Recovery" $ do
  withSystemTempDirectory "vibekv-test" $ \tempDir -> do
    let filePath = tempDir </> "vibekv.log"
    
    -- Create a new file store
    store1 <- newFileStore filePath
    
    -- Add multiple key-value pairs
    let keys = [1, 2, 3, 4, 5]
        values = [10, 20, 30, 40, 50]
    
    store1' <- foldM (\s (k, v) -> put s k v) store1 (zip keys values)
    
    -- Update a key
    store1'' <- put store1' 2 25
    
    -- Delete a key
    Right store1''' <- delete store1'' 3
    
    -- Close the store
    close store1'''
    
    -- Create a new store with the same file
    store2 <- newFileStore filePath
    
    -- Verify the data was recovered correctly
    -- Check the updated key
    result1 <- get store2 2
    case result1 of
      Right value -> 
        assertEqual "Updated value should be 25" 25 value
      Left KeyNotFound -> 
        assertFailure "Updated key should exist after recovery"
    
    -- Check the deleted key
    result2 <- get store2 3
    case result2 of
      Right _ -> 
        assertFailure "Deleted key should not exist after recovery"
      Left KeyNotFound -> 
        return ()
    
    -- Check other keys
    let expectedValues = [(1, 10), (4, 40), (5, 50)]
    
    forM_ expectedValues $ \(k, expectedValue) -> do
      result <- get store2 k
      case result of
        Right value -> 
          assertEqual ("Value for key " ++ show k) expectedValue value
        Left KeyNotFound -> 
          assertFailure ("Key " ++ show k ++ " should exist after recovery")
    
    -- Close the store
    close store2

-- | Helper function to create a FileStore in a temporary directory for testing
withTempFileStore :: (FileStore -> (FileStore -> IO ()) -> TestTree) -> TestTree
withTempFileStore f = do
  let setup = do
        tempDir <- getCanonicalTemporaryDirectory >>= flip createTempDirectory "vibekv-test"
        let filePath = tempDir </> "vibekv.log"
        store <- newFileStore filePath
        let cleanup s = do
              close s
              removeDirectoryRecursive tempDir
        return (store, cleanup)
  
  withResource setup (\_ -> return ()) $ \getResource ->
    testCase "FileStore" $ do
      (store, cleanup) <- getResource
      let test = f store cleanup
      return test

-- Utility functions
foldM :: Monad m => (b -> a -> m b) -> b -> [a] -> m b
foldM _ z [] = return z
foldM f z (x:xs) = do
  z' <- f z x
  foldM f z' xs

forM_ :: Monad m => [a] -> (a -> m b) -> m ()
forM_ xs f = mapM_ f xs
