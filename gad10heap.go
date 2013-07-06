package main

import (
	"fmt"
)

type BinaryHeap struct {
	elements []int
}

func (b *BinaryHeap) Build(entries []int) {
	b.elements = append(b.elements, entries...)
	var until = len(entries) / 2
	for i := 0; i < until; i++ {
	}
}

func (b *BinaryHeap) String() string {
	return fmt.Sprint(b.elements)
}

func main() {
	a := []int{12, 9, 8, 7, 6, 4, 3, 2}
	b := []int{-24, 16, -302, -17, -56, 8, -1, 22, 54, 92, -70, 33, 61}
	c := []int{1, 1, 1, 1, 1, 1, 1, 1}
	test_heap := new(BinaryHeap)
	test_heap.Build(a)
	fmt.Println(test_heap)
	fmt.Println(b)
	fmt.Println(c)
}
