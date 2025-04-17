# VibeKV - Haskell Implementation

A durable, resilient key-value store implemented in Haskell.

## Features

- In-memory key-value store
- File-based durable key-value store with transaction log
- Recovery from transaction log on startup
- 64-bit keys and values

## Building

```bash
cd haskell
cabal build
```

## Testing

```bash
cd haskell
cabal test
```

## Usage

```haskell
import VibeKV.Store
import VibeKV.MemoryStore
import VibeKV.FileStore

main :: IO ()
main = do
  -- Create an in-memory store
  memStore <- newMemoryStore
  
  -- Put a key-value pair
  memStore' <- put memStore 42 123
  
  -- Get the value
  result <- get memStore' 42
  case result of
    Right value -> putStrLn $ "Value: " ++ show value
    Left KeyNotFound -> putStrLn "Key not found"
  
  -- Create a file-based store
  fileStore <- newFileStore "vibekv.log"
  
  -- Put a key-value pair
  fileStore' <- put fileStore 42 123
  
  -- Get the value
  result' <- get fileStore' 42
  case result' of
    Right value -> putStrLn $ "Value: " ++ show value
    Left KeyNotFound -> putStrLn "Key not found"
  
  -- Close the stores
  close memStore'
  close fileStore'
```
