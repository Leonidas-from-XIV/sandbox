package main

import (
	"fmt"
	"math/rand"
	"time"
)

func revmerge(a []int, b []int) []int {
	if len(a) == 0 {
		// as this is revmerge, it needs to be reversed
		reverse(b)
		return b
	}
	if len(b) == 0 {
		reverse(a)
		return a
	}
	if a[0] <= b[0] {
		return append(revmerge(a[1:], b), a[0])
	}
	return append(revmerge(a, b[1:]), b[0])
}

func reverse(res []int) {
	// from Effective Go, Y U NO INCLUDE REVERSE?
	for i, j := 0, len(res)-1; i < j; i, j = i+1, j-1 {
		res[i], res[j] = res[j], res[i]
	}
}

func merge(a []int, b []int) []int {
	res := revmerge(a, b)
	reverse(res)
	return res
}

func mergesort(input []int) []int {
	// create a copy as slices are mutable
	in := make([]int, len(input))
	copy(in, input)

	if len(in) <= 1 {
		return in
	}
	if len(in) == 2 {
		if in[0] > in[1] {
			// invert first with second
			return []int{in[1], in[0]}
		}
		// sorted already
		return in
	}
	half := len(in) / 2
	left := in[:half]
	right := in[half:]
	return merge(mergesort(left), mergesort(right))
}

func main() {
	elements := make([]int, 20)
	r := rand.New(rand.NewSource(time.Now().UnixNano()))

	for k, _ := range elements {
		elements[k] = int(r.Float64() * 100)
	}
	fmt.Printf("Unsorted: %s\n", elements)
	fmt.Printf("Sorted:   %s\n", mergesort(elements))
}
