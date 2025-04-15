package store

import (
	"os"
	"path/filepath"
	"testing"
)

// fileStoreFactory creates a new FileStore instance in a temporary directory
func fileStoreFactory() Store {
	// Create a temporary directory for the test
	tempDir, err := os.MkdirTemp("", "vibekv-test-*")
	if err != nil {
		panic(err)
	}

	// Create a file store in the temporary directory
	filePath := filepath.Join(tempDir, "vibekv.log")
	store, err := NewFileStore(filePath)
	if err != nil {
		os.RemoveAll(tempDir)
		panic(err)
	}

	// Return a wrapped store that cleans up the temporary directory on close
	return &cleanupStore{
		Store:   store,
		tempDir: tempDir,
	}
}

// cleanupStore wraps a Store and cleans up temporary files on close
type cleanupStore struct {
	Store
	tempDir string
}

// Close closes the underlying store and cleans up temporary files
func (s *cleanupStore) Close() error {
	err := s.Store.Close()
	os.RemoveAll(s.tempDir)
	return err
}

// TestFileStorePutGet tests the basic Put and Get operations for FileStore
func TestFileStorePutGet(t *testing.T) {
	TestPutGet(t, fileStoreFactory)
}

// TestFileStoreDelete tests deleting an existing key for FileStore
func TestFileStoreDelete(t *testing.T) {
	TestDelete(t, fileStoreFactory)
}

// TestFileStoreDeleteNonExistentKey tests deleting a non-existent key for FileStore
func TestFileStoreDeleteNonExistentKey(t *testing.T) {
	TestDeleteNonExistentKey(t, fileStoreFactory)
}

// TestFileStoreRecovery tests that the store can recover its state from the transaction log
func TestFileStoreRecovery(t *testing.T) {
	// Create a temporary directory for the test
	tempDir, err := os.MkdirTemp("", "vibekv-recovery-test-*")
	if err != nil {
		t.Fatalf("Failed to create temporary directory: %v", err)
	}
	defer os.RemoveAll(tempDir)

	filePath := filepath.Join(tempDir, "vibekv.log")

	// Create a new file store
	store1, err := NewFileStore(filePath)
	if err != nil {
		t.Fatalf("Failed to create file store: %v", err)
	}

	// Add some data
	keys := []uint64{1, 2, 3, 4, 5}
	values := []uint64{10, 20, 30, 40, 50}

	for i, key := range keys {
		if err := store1.Put(key, values[i]); err != nil {
			t.Fatalf("Failed to put key-value pair: %v", err)
		}
	}

	// Delete a key
	if err := store1.Delete(keys[2]); err != nil {
		t.Fatalf("Failed to delete key: %v", err)
	}

	// Close the store
	if err := store1.Close(); err != nil {
		t.Fatalf("Failed to close store: %v", err)
	}

	// Create a new store with the same file
	store2, err := NewFileStore(filePath)
	if err != nil {
		t.Fatalf("Failed to create file store: %v", err)
	}
	defer store2.Close()

	// Verify the data was recovered
	for i, key := range keys {
		if key == keys[2] {
			// This key was deleted
			_, err := store2.Get(key)
			if err == nil {
				t.Errorf("Expected key %d to be deleted, but it was found", key)
			}
			if err != ErrKeyNotFound {
				t.Errorf("Expected ErrKeyNotFound, but got: %v", err)
			}
		} else {
			// This key should exist
			value, err := store2.Get(key)
			if err != nil {
				t.Errorf("Failed to get key %d: %v", key, err)
			} else if value != values[i] {
				t.Errorf("Value for key %d is %d, expected %d", key, value, values[i])
			}
		}
	}
}
