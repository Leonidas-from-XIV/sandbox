package main

import (
	"fmt"
)

type BinaryHeap struct {
	elements []int
}

func (b *BinaryHeap) bubble_up(i int) {
	H := b.elements
	for i > 0 || H[(i - 1) / 2] > H[i] {
		H[i], H[(i - 1) / 2] = H[(i - 1) / 2], H[i]
		i = (i - 1) / 2
	}
}

func (b *BinaryHeap) bubble_down(i int) {
	var m int
	n := len(b.elements)
	H := b.elements
	for 2*i + 1 < n {
		if (2*i + 2 >= n) {
			m = 2 * i + 1
		} else {
			if H[2*i + 1] < H[2*i + 2] {
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

func (b *BinaryHeap) Build(entries []int) {
	b.elements = append(b.elements, entries...)
	var until = len(entries) / 2
	for i := until - 1; i >= 0; i-- {
		//println("Processing", b.elements[i])
		b.bubble_down(i)
	}
}

func (b *BinaryHeap) Insert(v int) {
	b.elements = append(b.elements, v)
	b.bubble_up(len(b.elements) - 1)
}

func (b *BinaryHeap) String() string {
	return fmt.Sprint(b.elements)
}

func (b *BinaryHeap) DeleteMin() int {
	H := b.elements
	n := len(H)
	e := H[0]
	H[0] = H[n-1]
	// shrink slice by one element
	H = H[:n-1]
	b.bubble_down(0)
	return e
}

func (b *BinaryHeap) Min() int {
	return b.elements[0]
}

func main() {
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
	fmt.Println(b)
	fmt.Println(c)
}
