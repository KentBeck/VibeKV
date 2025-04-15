package store

import (
	"errors"
	"sync"
)

// Common errors
var (
	ErrKeyNotFound = errors.New("key not found")
)

// Store defines the interface for key-value stores
type Store interface {
	// Put stores a key-value pair
	Put(key uint64, value uint64) error

	// Get retrieves a value by key
	Get(key uint64) (uint64, error)

	// Delete removes a key-value pair
	Delete(key uint64) error

	// Close closes the store and releases any resources
	Close() error
}

// MemoryStore is an in-memory implementation of the Store interface
type MemoryStore struct {
	mu    sync.RWMutex
	data  map[uint64]uint64
}

// NewMemoryStore creates a new in-memory store
func NewMemoryStore() *MemoryStore {
	return &MemoryStore{
		data: make(map[uint64]uint64),
	}
}

// Put stores a key-value pair
func (s *MemoryStore) Put(key uint64, value uint64) error {
	s.mu.Lock()
	defer s.mu.Unlock()

	s.data[key] = value
	return nil
}

// Get retrieves a value by key
func (s *MemoryStore) Get(key uint64) (uint64, error) {
	s.mu.RLock()
	defer s.mu.RUnlock()

	value, exists := s.data[key]
	if !exists {
		return 0, ErrKeyNotFound
	}

	return value, nil
}

// Delete removes a key-value pair
func (s *MemoryStore) Delete(key uint64) error {
	s.mu.Lock()
	defer s.mu.Unlock()

	if _, exists := s.data[key]; !exists {
		return ErrKeyNotFound
	}

	delete(s.data, key)
	return nil
}

// Close is a no-op for MemoryStore
func (s *MemoryStore) Close() error {
	return nil
}
