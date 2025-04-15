package store

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
