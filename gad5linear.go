package main

import "fmt"

const size = 11

func h5(x int) int {
	return (5 * x) % size
}

func printmap(m *[size]int) {
	for i, _ := range m {
		fmt.Printf(" %2d ", i)
	}
	println()
	for _, e := range m {
		fmt.Printf(" %2d ", e)
	}
	println()
}

func insert(m *[size]int, e int) {
	bucket := findBucket(*m, e)
	m[bucket] = e
}

/* returns the next free bucket for value e */
func findBucket(m [size]int, e int) int {
	// ideal bucket
	bucket := h5(e)
	// skip until bucket is free
	for ; m[bucket] != 0 && m[bucket] != e; bucket = (bucket + 1) % 11 {
	}
	// return bucket number that was found
	return bucket
}

func delete(m *[size]int, e int) {
	bucket := findBucket(*m, e)
	m[bucket] = 0
	reflow(m, bucket)
}

func reflow(m *[size]int, start int) {
	for j := start + 1; j != start; j = (j + 1) % 11 {
		currentContent := m[j]
		// i = "ideal" bucket, j = current bucket
		i := h5(currentContent)
		for c := i; c != j; c = (c + 1) % 11 {
			if m[c] == 0 {
				// move to new location
				//println("Moving", currentContent)
				m[c] = currentContent
				m[j] = 0
				// location found, no need to check further
				break
			}
		}
	}
}

func main() {
	m := new([size]int)

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
	println("===========================================")
	delete(m, 23)
	printmap(m)
	println("===========================================")
	insert(m, 25)
	printmap(m)
}
