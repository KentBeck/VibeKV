package store

import (
	"encoding/binary"
	"fmt"
	"os"
	"sync"
)

// Operation types for the transaction log
const (
	OpPut    byte = 1
	OpDelete byte = 2
)

// FileStore is a file-based implementation of the Store interface
// It uses a transaction log to record all operations
type FileStore struct {
	mu       sync.RWMutex
	data     map[uint64]uint64 // In-memory cache of the current state
	file     *os.File          // File handle for the transaction log
	filePath string            // Path to the transaction log file
}

// NewFileStore creates a new file-based store
// It will create a new transaction log file if it doesn't exist,
// or recover the state from an existing file
func NewFileStore(filePath string) (Store, error) {
	// Open the file for reading and writing, create if it doesn't exist
	file, err := os.OpenFile(filePath, os.O_RDWR|os.O_CREATE, 0644)
	if err != nil {
		return nil, fmt.Errorf("failed to open file: %w", err)
	}

	store := &FileStore{
		data:     make(map[uint64]uint64),
		file:     file,
		filePath: filePath,
	}

	// Recover state from the transaction log
	if err := store.recover(); err != nil {
		file.Close()
		return nil, fmt.Errorf("failed to recover state: %w", err)
	}

	return store, nil
}

// recover reads the transaction log and rebuilds the in-memory state
func (s *FileStore) recover() error {
	// Get file info to check size
	fileInfo, err := s.file.Stat()
	if err != nil {
		return fmt.Errorf("failed to get file info: %w", err)
	}

	// If file is empty, nothing to recover
	if fileInfo.Size() == 0 {
		return nil
	}

	// Seek to the beginning of the file
	if _, err := s.file.Seek(0, 0); err != nil {
		return fmt.Errorf("failed to seek to beginning of file: %w", err)
	}

	// Read and process each record
	for {
		// Read operation type
		opTypeBuf := make([]byte, 1)
		n, err := s.file.Read(opTypeBuf)
		if err != nil || n == 0 {
			break // End of file or error
		}

		opType := opTypeBuf[0]

		// Read key
		keyBuf := make([]byte, 8)
		n, err = s.file.Read(keyBuf)
		if err != nil || n != 8 {
			return fmt.Errorf("failed to read key: %w", err)
		}
		key := binary.BigEndian.Uint64(keyBuf)

		switch opType {
		case OpPut:
			// Read value
			valueBuf := make([]byte, 8)
			n, err = s.file.Read(valueBuf)
			if err != nil || n != 8 {
				return fmt.Errorf("failed to read value: %w", err)
			}
			value := binary.BigEndian.Uint64(valueBuf)
			s.data[key] = value

		case OpDelete:
			delete(s.data, key)

		default:
			return fmt.Errorf("unknown operation type: %d", opType)
		}
	}

	// Seek to the end of the file for appending new records
	if _, err := s.file.Seek(0, 2); err != nil {
		return fmt.Errorf("failed to seek to end of file: %w", err)
	}

	return nil
}

// appendLogRecord writes a record to the transaction log
func (s *FileStore) appendLogRecord(opType byte, key uint64, value uint64) error {
	// Create the record buffer
	var buf []byte

	if opType == OpPut {
		buf = make([]byte, 17) // 1 byte for op type + 8 bytes for key + 8 bytes for value
		buf[0] = opType
		binary.BigEndian.PutUint64(buf[1:9], key)
		binary.BigEndian.PutUint64(buf[9:17], value)
	} else {
		buf = make([]byte, 9) // 1 byte for op type + 8 bytes for key
		buf[0] = opType
		binary.BigEndian.PutUint64(buf[1:9], key)
	}

	// Write the record to the file
	if _, err := s.file.Write(buf); err != nil {
		return fmt.Errorf("failed to write to log: %w", err)
	}

	// Sync to ensure durability
	if err := s.file.Sync(); err != nil {
		return fmt.Errorf("failed to sync log: %w", err)
	}

	return nil
}

// Put stores a key-value pair
func (s *FileStore) Put(key uint64, value uint64) error {
	s.mu.Lock()
	defer s.mu.Unlock()

	// Append to the transaction log first
	if err := s.appendLogRecord(OpPut, key, value); err != nil {
		return err
	}

	// Update in-memory state
	s.data[key] = value
	return nil
}

// Get retrieves a value by key
func (s *FileStore) Get(key uint64) (uint64, error) {
	s.mu.RLock()
	defer s.mu.RUnlock()

	value, exists := s.data[key]
	if !exists {
		return 0, ErrKeyNotFound
	}

	return value, nil
}

// Delete removes a key-value pair
func (s *FileStore) Delete(key uint64) error {
	s.mu.Lock()
	defer s.mu.Unlock()

	// Check if the key exists
	if _, exists := s.data[key]; !exists {
		return ErrKeyNotFound
	}

	// Append to the transaction log first
	if err := s.appendLogRecord(OpDelete, key, 0); err != nil {
		return err
	}

	// Update in-memory state
	delete(s.data, key)
	return nil
}

// Close closes the store and releases any resources
func (s *FileStore) Close() error {
	s.mu.Lock()
	defer s.mu.Unlock()

	if s.file != nil {
		if err := s.file.Close(); err != nil {
			return fmt.Errorf("failed to close file: %w", err)
		}
		s.file = nil
	}

	return nil
}
