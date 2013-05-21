package main

import (
	"container/list"
	"fmt"
)

const size = 11

func h5(x int) int {
	return (5 * x) % size
}

func printmap(m [size]*list.List) {
	for i, e := range m {
		fmt.Printf("[%2d]", i)
		for n := e.Front(); n != nil; n = n.Next() {
			fmt.Printf(" %2d ", n.Value)
		}
		fmt.Println()
	}
}

func insert(m [size]*list.List, e int) {
	bucket := h5(e)
	m[bucket].PushBack(e)
}

func delete(m [size]*list.List, e int) {
	bucket := h5(e)
	for n := m[bucket].Back(); n != nil; n = n.Prev() {
		// iterate over linked list and delete all occurences of e
		if e == n.Value {
			m[bucket].Remove(n)
		}
	}
}

func main() {
	// create and initialize array of lists
	var m [size]*list.List
	for index, _ := range m {
		m[index] = list.New()
	}

	insert(m, 3)
	insert(m, 11)
	insert(m, 9)
	insert(m, 7)
	insert(m, 14)
	insert(m, 23)
	insert(m, 4)
	insert(m, 12)
	insert(m, 15)
	insert(m, 8)
	insert(m, 1)
	printmap(m)
	println("============")
	delete(m, 23)
	printmap(m)
	println("============")
	insert(m, 25)
	printmap(m)
}
