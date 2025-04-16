package store

import (
	"testing"
)

// TestMemoryStorePut tests the Put operation for MemoryStore
func TestMemoryStorePut(t *testing.T) {
	store := NewMemoryStore()
	defer store.Close()
	TestPut(t, store)
}

// TestMemoryStoreGet tests the Get operation for MemoryStore
func TestMemoryStoreGet(t *testing.T) {
	store := NewMemoryStore()
	defer store.Close()
	TestGet(t, store)
}

// TestMemoryStoreDelete tests deleting an existing key for MemoryStore
func TestMemoryStoreDelete(t *testing.T) {
	store := NewMemoryStore()
	defer store.Close()
	TestDelete(t, store)
}

// TestMemoryStoreDeleteNonExistentKey tests deleting a non-existent key for MemoryStore
func TestMemoryStoreDeleteNonExistentKey(t *testing.T) {
	store := NewMemoryStore()
	defer store.Close()
	TestDeleteNonExistentKey(t, store)
}
