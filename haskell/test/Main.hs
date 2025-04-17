module Main where

import Test.Tasty
import qualified VibeKV.StoreTest as StoreTest
import qualified VibeKV.MemoryStoreTest as MemoryStoreTest
import qualified VibeKV.FileStoreTest as FileStoreTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "VibeKV Tests"
  [ StoreTest.tests
  , MemoryStoreTest.tests
  , FileStoreTest.tests
  ]
