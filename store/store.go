package store

import (
	"errors"
	"sync"
)

// Common errors
var (
	ErrKeyNotFound = errors.New("key not found")
)

// Store represents an in-memory key-value store
type Store struct {
	mu    sync.RWMutex
	data  map[uint64]uint64
}

// NewStore creates a new in-memory store
func NewStore() *Store {
	return &Store{
		data: make(map[uint64]uint64),
	}
}

// Put stores a key-value pair
func (s *Store) Put(key uint64, value uint64) error {
	s.mu.Lock()
	defer s.mu.Unlock()
	
	s.data[key] = value
	return nil
}

// Get retrieves a value by key
func (s *Store) Get(key uint64) (uint64, error) {
	s.mu.RLock()
	defer s.mu.RUnlock()
	
	value, exists := s.data[key]
	if !exists {
		return 0, ErrKeyNotFound
	}
	
	return value, nil
}

// Delete removes a key-value pair
func (s *Store) Delete(key uint64) error {
	s.mu.Lock()
	defer s.mu.Unlock()
	
	if _, exists := s.data[key]; !exists {
		return ErrKeyNotFound
	}
	
	delete(s.data, key)
	return nil
}
