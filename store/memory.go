package store

import (
	"errors"
	"sync"
)

// Common errors
var (
	ErrKeyNotFound = errors.New("key not found")
)

// MemoryStore is an in-memory implementation of the Store interface
type MemoryStore struct {
	mu   sync.RWMutex
	data map[uint64]uint64
}

// NewMemoryStore creates a new in-memory store
func NewMemoryStore() Store {
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

// Close is a no-op for MemoryStore as it doesn't use external resources
func (s *MemoryStore) Close() error {
	return nil
}
