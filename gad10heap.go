// Binary Heap implementation
package main

import (
	"fmt"
)

type BinaryHeap struct {
	elements []int
}

// moves an element up the heap and returns its final index in the heap slice
func (b *BinaryHeap) bubble_up(i int) int {
	H := b.elements
	for i > 0 && H[(i-1)/2] > H[i] {
		//println(H[(i-1) / 2], ">", H[i])
		H[i], H[(i-1)/2] = H[(i-1)/2], H[i]
		i = (i - 1) / 2
	}
	return i
}

// moves an element down the heap
func (b *BinaryHeap) bubble_down(i int) {
	var m int
	n := len(b.elements)
	H := b.elements
	for 2*i+1 < n {
		if 2*i+2 >= n {
			m = 2*i + 1
		} else {
			if H[2*i+1] < H[2*i+2] {
				m = 2*i + 1
			} else {
				m = 2*i + 2
			}
		}
		if H[i] <= H[m] {
			return
		}
		H[i], H[m] = H[m], H[i]
		i = m
	}
}

// builds a heap from a slice of items
func (b *BinaryHeap) Build(entries []int) {
	b.elements = append(b.elements, entries...)
	// bubble down the first floor(n/2) elements in reverse order
	var until = len(entries) / 2
	for i := until - 1; i >= 0; i-- {
		//println("Processing", b.elements[i])
		b.bubble_down(i)
	}
}

// inserts an element into the heap
func (b *BinaryHeap) Insert(v int) {
	// use InsertH and ignore its return value
	b.InsertH(v)
}

// inserts an element into the heap and returns its resulting index
func (b *BinaryHeap) InsertH(v int) int {
	b.elements = append(b.elements, v)
	return b.bubble_up(len(b.elements) - 1)
}

// printing a heap should just return the slice printed
func (b *BinaryHeap) String() string {
	return fmt.Sprint(b.elements)
}

// deletes the minimal element
func (b *BinaryHeap) DeleteMin() int {
	// … by delegating to removeHV and setting the handle to a 0 since
	// thats where the minimum is, duh
	return b.removeHV(0)
}

// returns the value of the element at index h and deletes it
func (b *BinaryHeap) removeHV(h int) int {
	H := b.elements
	n := len(H)
	// the element to be removed
	e := H[h]
	// overwrite it with the last element
	H[h] = H[n-1]
	// shrink slice by one element, thus deleting the last
	b.elements = H[:n-1]
	// trickle down the element that was moved
	b.bubble_down(h)
	// return previous value
	return e
}

// removes an element at index h
func (b *BinaryHeap) RemoveH(h int) {
	// … by ignoring removeHV's return value,
	// ain't nobody got time for that!
	b.removeHV(h)
}

// returns the value of the minimal value
func (b *BinaryHeap) Min() int {
	return b.elements[0]
}

// decreases a the value at index h to value k
func (b *BinaryHeap) DecreaseKeyH(h int, k int) {
	H := b.elements
	if H[h] < k {
		panic("Key to decrease is lower than new value")
	}
	H[h] = k
	b.bubble_up(h)
}

func main() {
	// test data
	a := []int{12, 9, 8, 7, 6, 4, 3, 2}
	b := []int{-24, 16, -302, -17, -56, 8, -1, 22, 54, 92, -70, 33, 61}
	c := []int{1, 1, 1, 1, 1, 1, 1, 1}
	test_heap := new(BinaryHeap)
	test_heap.Build(a)
	fmt.Println("test_heap.Build(a), should be [2 6 3 7 12 4 8 9], is ", test_heap)
	test_heap.Insert(1)
	fmt.Println("test_heap.Insert(1), should be [1 2 3 6 12 4 8 9 7], is ", test_heap)
	test_heap.DeleteMin()
	fmt.Println("test_heap.DeleteMin(), should be [2 6 3 7 12 4 8 9], is ", test_heap)
	test_heap.DecreaseKeyH(4, 5)
	fmt.Println("test_heap.DecreaseKeyH(4, 5), should be [2 5 3 7 6 4 8 9], is ", test_heap)
	test_heap2 := new(BinaryHeap)
	test_heap2.Build(b)
	fmt.Println("test_heap2 with negative keys: ", test_heap2)
	test_heap3 := new(BinaryHeap)
	test_heap3.Build(c)
	fmt.Println("test_heap3 with same keys: ", test_heap3)
}
