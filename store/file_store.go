package store

import (
	"bufio"
	"encoding/binary"
	"fmt"
	"io"
	"os"
	"path/filepath"
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
	mu         sync.RWMutex
	data       map[uint64]uint64 // In-memory cache of the current state
	logFile    *os.File          // File handle for the transaction log
	logWriter  *bufio.Writer     // Buffered writer for the transaction log
	filePath   string            // Path to the transaction log file
}

// NewFileStore creates a new file-based store
// It will create a new transaction log file if it doesn't exist,
// or recover the state from an existing file
func NewFileStore(filePath string) (*FileStore, error) {
	// Create directory if it doesn't exist
	dir := filepath.Dir(filePath)
	if err := os.MkdirAll(dir, 0755); err != nil {
		return nil, fmt.Errorf("failed to create directory: %w", err)
	}

	// Open the file for reading and writing, create if it doesn't exist
	file, err := os.OpenFile(filePath, os.O_RDWR|os.O_CREATE, 0644)
	if err != nil {
		return nil, fmt.Errorf("failed to open file: %w", err)
	}

	store := &FileStore{
		data:      make(map[uint64]uint64),
		logFile:   file,
		logWriter: bufio.NewWriter(file),
		filePath:  filePath,
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
	// Seek to the beginning of the file
	if _, err := s.logFile.Seek(0, io.SeekStart); err != nil {
		return fmt.Errorf("failed to seek to beginning of file: %w", err)
	}

	reader := bufio.NewReader(s.logFile)

	for {
		// Read the operation type
		opType, err := reader.ReadByte()
		if err == io.EOF {
			break // End of file
		}
		if err != nil {
			return fmt.Errorf("failed to read operation type: %w", err)
		}

		// Read the key (8 bytes)
		keyBuf := make([]byte, 8)
		if _, err := io.ReadFull(reader, keyBuf); err != nil {
			if err == io.EOF || err == io.ErrUnexpectedEOF {
				// Partial record at the end of the file, ignore it
				break
			}
			return fmt.Errorf("failed to read key: %w", err)
		}
		key := binary.BigEndian.Uint64(keyBuf)

		switch opType {
		case OpPut:
			// Read the value (8 bytes)
			valueBuf := make([]byte, 8)
			if _, err := io.ReadFull(reader, valueBuf); err != nil {
				if err == io.EOF || err == io.ErrUnexpectedEOF {
					// Partial record at the end of the file, ignore it
					break
				}
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
	if _, err := s.logFile.Seek(0, io.SeekEnd); err != nil {
		return fmt.Errorf("failed to seek to end of file: %w", err)
	}

	return nil
}

// appendLogRecord writes a record to the transaction log
func (s *FileStore) appendLogRecord(opType byte, key uint64, value uint64) error {
	var buf [17]byte // 1 byte for op type + 8 bytes for key + 8 bytes for value
	buf[0] = opType
	binary.BigEndian.PutUint64(buf[1:9], key)

	var n int
	if opType == OpPut {
		binary.BigEndian.PutUint64(buf[9:17], value)
		n = 17
	} else {
		n = 9
	}

	if _, err := s.logWriter.Write(buf[:n]); err != nil {
		return fmt.Errorf("failed to write to log: %w", err)
	}

	// Flush to ensure durability
	if err := s.logWriter.Flush(); err != nil {
		return fmt.Errorf("failed to flush log: %w", err)
	}

	// Sync to disk
	if err := s.logFile.Sync(); err != nil {
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

	// Flush any buffered writes
	if err := s.logWriter.Flush(); err != nil {
		return fmt.Errorf("failed to flush log: %w", err)
	}

	// Close the file
	if err := s.logFile.Close(); err != nil {
		return fmt.Errorf("failed to close file: %w", err)
	}

	return nil
}
