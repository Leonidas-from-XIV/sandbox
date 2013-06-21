package main

import (
	"fmt"
	"math/rand"
	"time"
)

func quicksort(ele []int) []int {
	if len(ele) <= 1 {
		return ele
	}
	pivot := ele[0]
	var smaller []int
	var larger []int
	for _, v := range ele[1:] {
		if v >= pivot {
			larger = append(larger, v)
		} else {
			smaller = append(smaller, v)
		}
	}

	sorted := quicksort(smaller)
	sorted = append(sorted, pivot)
	sorted = append(sorted, quicksort(larger)...)

	return sorted
}

func main() {
	elements := make([]int, 10)
	r := rand.New(rand.NewSource(time.Now().UnixNano()))
	for k, _ := range elements {
		elements[k] = int(r.Float64() * 30)
	}
	fmt.Println("Unsorted:\t", elements)
	sorted := quicksort(elements)
	fmt.Println("Sorted:\t\t", sorted)
}
