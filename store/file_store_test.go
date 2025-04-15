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

// TestFileStorePut tests the Put operation for FileStore
func TestFileStorePut(t *testing.T) {
	TestPut(t, fileStoreFactory)
}

// TestFileStoreGet tests the Get operation for FileStore
func TestFileStoreGet(t *testing.T) {
	TestGet(t, fileStoreFactory)
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
	key := uint64(42)
	value := uint64(123)

	if err := store1.Put(key, value); err != nil {
		t.Fatalf("Failed to put key-value pair: %v", err)
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
	retrievedValue, err := store2.Get(key)
	if err != nil {
		t.Fatalf("Failed to get value for key: %v", err)
	}

	if retrievedValue != value {
		t.Errorf("Retrieved value %d does not match original value %d", retrievedValue, value)
	}
}

// TestFileStoreComplexRecovery tests recovery with multiple operations including deletions
func TestFileStoreComplexRecovery(t *testing.T) {
	// Create a temporary directory for the test
	tempDir, err := os.MkdirTemp("", "vibekv-complex-recovery-test-*")
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

	// Add multiple key-value pairs
	keys := []uint64{1, 2, 3, 4, 5}
	values := []uint64{10, 20, 30, 40, 50}

	for i, key := range keys {
		if err := store1.Put(key, values[i]); err != nil {
			t.Fatalf("Failed to put key-value pair: %v", err)
		}
	}

	// Update a key
	if err := store1.Put(keys[1], 25); err != nil {
		t.Fatalf("Failed to update key: %v", err)
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

	// Verify the data was recovered correctly
	// Check the updated key
	value, err := store2.Get(keys[1])
	if err != nil {
		t.Fatalf("Failed to get updated key: %v", err)
	}
	if value != 25 {
		t.Errorf("Retrieved value %d for updated key does not match expected value 25", value)
	}

	// Check the deleted key
	_, err = store2.Get(keys[2])
	if err == nil {
		t.Errorf("Expected error when getting deleted key, but got nil")
	}

	// Check other keys
	expectedValues := map[uint64]uint64{
		1: 10,
		4: 40,
		5: 50,
	}

	for k, expectedValue := range expectedValues {
		value, err := store2.Get(k)
		if err != nil {
			t.Fatalf("Failed to get key %d: %v", k, err)
		}
		if value != expectedValue {
			t.Errorf("Retrieved value %d for key %d does not match expected value %d", value, k, expectedValue)
		}
	}
}
