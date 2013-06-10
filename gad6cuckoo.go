// yes, this is indeed incomplete. woe on me!
package main

import (
	"math"
)

type HashTable struct {
	one []int
	two []int
	h1  func(int) int
	h2  func(int) int
	max int
}

func (h *HashTable) find(e int) bool {
	return false
}

func (h *HashTable) insert(e int) {
}

func NewHashTable(a1 []int, k1 int, p1 int, n1 int, a2 []int, k2 int, p2 int, n2 int, max int) *HashTable {
	h := new(HashTable)
	h.max = max
	h.h1 = get_hash_fn(a1, k1, p1, n1)
	h.h2 = get_hash_fn(a2, k2, p2, n2)
	h.one = make([]int, n1)
	h.two = make([]int, n2)
	return h
}

func get_hash_fn(a []int, k int, p int, n int) func(int) int {
	return func(x int) int {
		sum := 0
		for j := 0; j <= k-1; j++ {
			sum += a[j] * int(math.Pow(float64(x), float64(j)))
		}
		return (sum % p) % n
	}
}

func main() {
	a1 := []int{4, 5, 8, 9}
	k1 := 4
	p1 := 9
	n1 := 7
	a2 := []int{4, 5, 8, 9}
	k2 := 4
	p2 := 9
	n2 := 7
	max := 3

	h := NewHashTable(a1, k1, p1, n1, a2, k2, p2, n2, max)
	h.insert(1)

}
