package store

import (
	"testing"
)

// StoreFactory is a function that creates a new Store instance
type StoreFactory func() Store

// TestPut tests the basic Put operation
func TestPut(t *testing.T, factory StoreFactory) {
	store := factory()
	defer store.Close()
	
	key := uint64(42)
	value := uint64(123)
	
	err := store.Put(key, value)
	if err != nil {
		t.Fatalf("Failed to put key-value pair: %v", err)
	}
}

// TestGet tests the Get operation
func TestGet(t *testing.T, factory StoreFactory) {
	store := factory()
	defer store.Close()
	
	key := uint64(42)
	value := uint64(123)
	
	// Put a value
	err := store.Put(key, value)
	if err != nil {
		t.Fatalf("Failed to put key-value pair: %v", err)
	}
	
	// Get the value
	retrievedValue, err := store.Get(key)
	if err != nil {
		t.Fatalf("Failed to get value for key: %v", err)
	}
	
	// Verify the retrieved value matches the original
	if retrievedValue != value {
		t.Errorf("Retrieved value %d does not match original value %d", retrievedValue, value)
	}
	
	// Try to get a non-existent key
	nonExistentKey := uint64(999)
	_, err = store.Get(nonExistentKey)
	if err == nil {
		t.Errorf("Expected error when getting non-existent key, but got nil")
	}
}

// TestDelete tests deleting an existing key
func TestDelete(t *testing.T, factory StoreFactory) {
	store := factory()
	defer store.Close()
	
	key := uint64(42)
	value := uint64(123)
	
	// Put a value
	err := store.Put(key, value)
	if err != nil {
		t.Fatalf("Failed to put key-value pair: %v", err)
	}
	
	// Delete the key
	err = store.Delete(key)
	if err != nil {
		t.Fatalf("Failed to delete key: %v", err)
	}
	
	// Try to get the deleted key
	_, err = store.Get(key)
	if err == nil {
		t.Errorf("Expected error when getting deleted key, but got nil")
	}
}

// TestDeleteNonExistentKey tests deleting a non-existent key
func TestDeleteNonExistentKey(t *testing.T, factory StoreFactory) {
	store := factory()
	defer store.Close()
	
	// Try to delete a non-existent key
	nonExistentKey := uint64(999)
	err := store.Delete(nonExistentKey)
	if err == nil {
		t.Errorf("Expected error when deleting non-existent key, but got nil")
	}
}
