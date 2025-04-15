package store

import (
	"testing"
)

// memoryStoreFactory creates a new MemoryStore instance
func memoryStoreFactory() Store {
	return NewMemoryStore()
}

// TestMemoryStorePut tests the Put operation for MemoryStore
func TestMemoryStorePut(t *testing.T) {
	TestPut(t, memoryStoreFactory)
}

// TestMemoryStoreGet tests the Get operation for MemoryStore
func TestMemoryStoreGet(t *testing.T) {
	TestGet(t, memoryStoreFactory)
}

// TestMemoryStoreDelete tests deleting an existing key for MemoryStore
func TestMemoryStoreDelete(t *testing.T) {
	TestDelete(t, memoryStoreFactory)
}

// TestMemoryStoreDeleteNonExistentKey tests deleting a non-existent key for MemoryStore
func TestMemoryStoreDeleteNonExistentKey(t *testing.T) {
	TestDeleteNonExistentKey(t, memoryStoreFactory)
}
