// is this working properly? Not entirely sure
package main

import (
	"bytes"
	"errors"
	"fmt"
	"math"
)

type HashTable struct {
	one   []int
	two   []int
	h1    func(int) int
	h2    func(int) int
	max   int
	first bool
}

func (h *HashTable) Find(e int) bool {
	return h.one[h.h1(e)] == e || h.two[h.h2(e)] == e
}

func (h *HashTable) Insert(e int) error {
	return h.insert(e, h.max)
}

func (h *HashTable) insert(e int, retries int) error {
	h1 := h.h1(e)
	h2 := h.h2(e)
	if h.one[h1] == e || h.two[h2] == e {
		return nil
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
		return nil
	} else {
		if retries == 0 {
			return errors.New(fmt.Sprintf("Rejected value %d, does not fit", e))
		}
		old := relevant[index]
		relevant[index] = e
		return h.insert(old, retries-1)
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
	buf.WriteString("Table 1: [ ")
	for _, v := range h.one {
		buf.WriteString(fmt.Sprintf("%2d ", v))
	}
	buf.WriteString("]\nTable 2: [ ")
	for _, v := range h.two {
		buf.WriteString(fmt.Sprintf("%2d ", v))
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
	a1 := []int{2, 4, 16, 8}
	k1 := len(a1)
	p1 := 64
	n1 := 20

	max := 3

	h := NewHashTable(a1, k1, p1, n1, a1, k1, p1, n1, max)
	fmt.Printf("1 exists in h? %t\n", h.Find(1))
	h.Insert(1)
	fmt.Printf("%s\n", h)
	fmt.Printf("1 exists in h? %t\n", h.Find(1))

	for i := 0; i < 40; i = i + 3 {
		fmt.Printf("Inserting %d\n", i)
		err := h.Insert(i)
		if err != nil {
			fmt.Printf("Error: %s\n", err)
		}
	}
	fmt.Printf("%s\n", h)

	a2 := []int{3, 19, 27, 31}
	k2 := len(a2)
	n2 := 53
	p2 := 29
	a3 := []int{2, 5, 17, 21}
	k3 := len(a3)
	n3 := 33
	p3 := 35

	fmt.Printf("New HashTable with better parameters\n")
	h2 := NewHashTable(a2, k2, p2, n2, a3, k3, p3, n3, max)
	for i := 0; i < 40; i = i + 3 {
		fmt.Printf("Inserting %d\n", i)
		err := h2.Insert(i)
		if err != nil {
			fmt.Printf("Error: %s\n", err)
		}
	}
	fmt.Printf("After inserting range(0, 40, 3):\n%s\n", h2)
	for i := 0; i < 40; i = i + 2 {
		fmt.Printf("Inserting %d\n", i)
		err := h2.Insert(i)
		if err != nil {
			fmt.Printf("Error: %s\n", err)
		}
	}
	fmt.Printf("After inserting range(0, 40, 2):\n%s\n", h2)

	for i := 0; i < 44; i = i + 2 {
		fmt.Printf("Trying to remove %d\n", i)
		h2.Remove(i)
	}
	fmt.Printf("After removing range(0, 44, 2):\n%s\n", h2)
}
