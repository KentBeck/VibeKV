package store

import (
	"testing"
)

func TestBasicPutGet(t *testing.T) {
	// Create a new in-memory store
	store := NewStore()
	
	// Test key and value (64-bit)
	key := uint64(42)
	value := uint64(123)
	
	// Put the key-value pair
	err := store.Put(key, value)
	if err != nil {
		t.Fatalf("Failed to put key-value pair: %v", err)
	}
	
	// Get the value by key
	retrievedValue, err := store.Get(key)
	if err != nil {
		t.Fatalf("Failed to get value for key: %v", err)
	}
	
	// Verify the retrieved value matches the original
	if retrievedValue != value {
		t.Errorf("Retrieved value %d does not match original value %d", retrievedValue, value)
	}
}
