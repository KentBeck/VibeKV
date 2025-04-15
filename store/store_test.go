package store

import (
	"testing"
)

// memoryStoreFactory creates a new MemoryStore instance
func memoryStoreFactory() Store {
	return NewMemoryStore()
}

// TestMemoryStorePutGet tests the basic Put and Get operations for MemoryStore
func TestMemoryStorePutGet(t *testing.T) {
	TestPutGet(t, memoryStoreFactory)
}

// TestMemoryStoreDelete tests deleting an existing key for MemoryStore
func TestMemoryStoreDelete(t *testing.T) {
	TestDelete(t, memoryStoreFactory)
}

// TestMemoryStoreDeleteNonExistentKey tests deleting a non-existent key for MemoryStore
func TestMemoryStoreDeleteNonExistentKey(t *testing.T) {
	TestDeleteNonExistentKey(t, memoryStoreFactory)
}
