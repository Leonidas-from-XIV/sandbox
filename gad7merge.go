package main

import (
	"fmt"
	"math/rand"
	"time"
)

func revmerge(a []int, b []int) []int {
	if len(a) == 0 {
		return b
	} else if len(b) == 0 {
		return a
	} else {
		if a[0] <= b[0] {
			return append(revmerge(a[1:], b), a[0])
		} else {
			return append(revmerge(a, b[1:]), b[0])
		}
	}
}

func merge(a []int, b []int) []int {
	res := revmerge(a, b)
	// from Effective Go, Y U NO INCLUDE REVERSE?
	for i, j := 0, len(res)-1; i < j; i, j = i+1, j-1 {
		res[i], res[j] = res[j], res[i]
	}
	return res
}

func mergesort(in []int) []int {
	return in
}

func main() {
	elements := make([]int, 20)
	r := rand.New(rand.NewSource(time.Now().UnixNano()))

	for k, _ := range elements {
		elements[k] = int(r.Float64() * 100)
	}
	fmt.Printf("%s\n", elements)
	fmt.Printf("Merged: %s\n", merge([]int{1, 3}, []int{4, 5}))
}
