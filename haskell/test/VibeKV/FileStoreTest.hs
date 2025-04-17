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
import Data.Word (Word64)
import System.IO.Unsafe (unsafePerformIO)

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
  -- Create a temporary directory for the test
  tempDir <- getCanonicalTemporaryDirectory >>= flip createTempDirectory "vibekv-recovery-test"
  let filePath = tempDir </> "vibekv.log"

  -- Write a key-value pair to a file
  writeKeyValueToFile filePath 42 123

  -- Read the key-value pair from the file
  value <- readKeyValueFromFile filePath 42

  -- Verify the value
  assertEqual "Retrieved value should match original" 123 value

  -- Clean up
  removeDirectoryRecursive tempDir

-- | Test recovery with multiple operations including deletions
testComplexRecovery :: TestTree
testComplexRecovery = testCase "FileStore - Complex Recovery" $ do
  -- Create a temporary directory for the test
  tempDir <- getCanonicalTemporaryDirectory >>= flip createTempDirectory "vibekv-complex-recovery-test"
  let filePath = tempDir </> "vibekv.log"

  -- Write multiple key-value pairs to a file
  writeMultipleKeyValuesToFile filePath [(1, 10), (2, 20), (3, 30), (4, 40), (5, 50)]

  -- Update a key
  updateKeyValueInFile filePath 2 25

  -- Delete a key
  deleteKeyFromFile filePath 3

  -- Read and verify the values
  value1 <- readKeyValueFromFile filePath 1
  assertEqual "Value for key 1" 10 value1

  value2 <- readKeyValueFromFile filePath 2
  assertEqual "Value for key 2 (updated)" 25 value2

  value4 <- readKeyValueFromFile filePath 4
  assertEqual "Value for key 4" 40 value4

  value5 <- readKeyValueFromFile filePath 5
  assertEqual "Value for key 5" 50 value5

  -- Verify the deleted key is gone
  assertKeyNotFound filePath 3

  -- Clean up
  removeDirectoryRecursive tempDir

-- | Helper function to create a FileStore in a temporary directory for testing
withTempFileStore :: (FileStore -> (FileStore -> IO ()) -> TestTree) -> TestTree
withTempFileStore f = unsafePerformIO $ do
  tempDir <- getCanonicalTemporaryDirectory >>= flip createTempDirectory "vibekv-test"
  let filePath = tempDir </> "vibekv.log"
  store <- newFileStore filePath
  let cleanup s = do
        close s
        removeDirectoryRecursive tempDir
  return (f store cleanup)

-- | Helper function to write a key-value pair to a file
writeKeyValueToFile :: FilePath -> Word64 -> Word64 -> IO ()
writeKeyValueToFile filePath key value = do
  store <- newFileStore filePath
  store' <- put store key value
  close store'

-- | Helper function to read a key-value pair from a file
readKeyValueFromFile :: FilePath -> Word64 -> IO Word64
readKeyValueFromFile filePath key = do
  store <- newFileStore filePath
  result <- get store key
  close store
  case result of
    Right value -> return value
    Left KeyNotFound -> assertFailure $ "Key " ++ show key ++ " not found"

-- | Helper function to write multiple key-value pairs to a file
writeMultipleKeyValuesToFile :: FilePath -> [(Word64, Word64)] -> IO ()
writeMultipleKeyValuesToFile filePath keyValues = do
  store <- newFileStore filePath
  store' <- foldM (\s (k, v) -> put s k v) store keyValues
  close store'

-- | Helper function to update a key-value pair in a file
updateKeyValueInFile :: FilePath -> Word64 -> Word64 -> IO ()
updateKeyValueInFile filePath key value = do
  store <- newFileStore filePath
  store' <- put store key value
  close store'

-- | Helper function to delete a key from a file
deleteKeyFromFile :: FilePath -> Word64 -> IO ()
deleteKeyFromFile filePath key = do
  store <- newFileStore filePath
  result <- delete store key
  close store
  case result of
    Right _ -> return ()
    Left KeyNotFound -> assertFailure $ "Key " ++ show key ++ " not found for deletion"

-- | Helper function to assert that a key is not found in a file
assertKeyNotFound :: FilePath -> Word64 -> IO ()
assertKeyNotFound filePath key = do
  store <- newFileStore filePath
  result <- get store key
  close store
  case result of
    Right value -> assertFailure $ "Key " ++ show key ++ " found with value " ++ show value
    Left KeyNotFound -> return ()



-- Utility functions
foldM :: Monad m => (b -> a -> m b) -> b -> [a] -> m b
foldM _ z [] = return z
foldM f z (x:xs) = do
  z' <- f z x
  foldM f z' xs

forM_ :: Monad m => [a] -> (a -> m b) -> m ()
forM_ xs f = mapM_ f xs
