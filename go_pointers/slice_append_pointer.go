/*
 .. in which case you have to know about slice gotchas
 */

package main

import "fmt"

func update(x *[]int) {
	*x = append(*x, 1)
}

func main() {
	x := []int{0}
	fmt.Printf("Before: %v\n", x)
	update(&x)
	fmt.Printf("After: %v\n", x)
}


