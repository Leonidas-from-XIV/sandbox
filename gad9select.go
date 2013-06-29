// QuickSelect implementation
// Basically, adopted from <http://pine.cs.yale.edu/pinewiki/QuickSelect>
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func quickselect(ele []int, k int) int {
	pivot := ele[0]
	var smaller []int
	var larger []int
	for _, v := range ele[1:] {
		if v > pivot {
			larger = append(larger, v)
		} else if v < pivot {
			smaller = append(smaller, v)
		}
	}

	if k <= len(smaller) {
		return quickselect(smaller, k)
	} else if k > len(ele) - len(larger) {
		return quickselect(larger, k - (len(ele) - len(larger)))
	} else {
		return pivot
	}
}

func main() {
	elements := make([]int, 10)
	r := rand.New(rand.NewSource(time.Now().UnixNano()))
	for k, _ := range elements {
		elements[k] = int(r.Float64() * 30)
	}
	fmt.Println("List:\t\t", elements)
	selected := quickselect(elements, 3)
	fmt.Println("3rd smallest:\t", selected)

}
