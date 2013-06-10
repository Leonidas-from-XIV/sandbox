// yes, this is indeed incomplete. woe on me!
package main

import (
	"fmt"
	"math"
	"bytes"
)

type HashTable struct {
	one []int
	two []int
	h1  func(int) int
	h2  func(int) int
	max int
	first bool
}

func (h *HashTable) Find(e int) bool {
	return h.one[h.h1(e)] == e || h.two[h.h2(e)] == e
}

func (h *HashTable) Insert(e int) {
	h.insert(e, h.max)
}

func (h *HashTable) insert(e int, retries int) {
	h1 := h.h1(e)
	h2 := h.h2(e)
	if h.one[h1] == e || h.two[h2] == e {
		return
	}

	h.first = !h.first
	var relevant []int
	var index int
	if h.first {
		relevant = h.one
		index = h1
	} else {
		relevant = h.two
		index = h2
	}

	if relevant[index] == 0 {
		relevant[index] = e
		return
	} else {
		println("Moving...")
		if retries == 0 {
			panic("Maximum retries for insertion reached")
		}
		old := relevant[index]
		relevant[index] = e
		h.insert(old, retries - 1)
	}

}

func (h *HashTable) Remove(e int) {
	h1 := h.h1(e)
	h2 := h.h2(e)
	if h.one[h1] != 0 {
		h.one[h1] = 0
	} else if h.two[h2] != 0 {
		h.two[h2] = 0
	}
}

func (h *HashTable) String() string {
	var buf bytes.Buffer
	buf.WriteString("Tabelle 1: [ ")
	for _, v := range(h.one) {
		buf.WriteString(fmt.Sprintf("%d ", v))
	}
	buf.WriteString("]\nTabelle 2: [ ")
	for _, v := range(h.two) {
		buf.WriteString(fmt.Sprintf("%d ", v))
	}
	buf.WriteString("]")
	return buf.String()
}

func NewHashTable(a1 []int, k1 int, p1 int, n1 int, a2 []int, k2 int, p2 int, n2 int, max int) *HashTable {
	h := new(HashTable)
	h.max = max
	h.h1 = get_hash_fn(a1, k1, p1, n1)
	h.h2 = get_hash_fn(a2, k2, p2, n2)
	h.one = make([]int, n1)
	h.two = make([]int, n2)
	h.first = false
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
	k1 := len(a1)
	p1 := 9
	n1 := 7
	a2 := []int{4, 5, 8, 9}
	k2 := len(a2)
	p2 := 9
	n2 := 7
	max := 3

	h := NewHashTable(a1, k1, p1, n1, a2, k2, p2, n2, max)
	fmt.Printf("1 exists in h? %t\n", h.Find(1))
	h.Insert(1)
	fmt.Printf("%s\n", h)
	fmt.Printf("1 exists in h? %t\n", h.Find(1))
	h.Remove(1)
}
