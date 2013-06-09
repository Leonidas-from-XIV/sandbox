// yes, this is indeed incomplete. woe on me!
package main

import (
	"math"
)

type HashTable struct {
	one []int
	two []int
}

func (h *HashTable) find(e int) bool {
	return false
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
	a := []int{0, 0, 0}
	get_hash_fn(a, 3, 0, 0)
}
