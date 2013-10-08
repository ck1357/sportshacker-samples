/*
 .. except when they're not :-(
 */

package main

import "fmt"

func update(x []int) {
	x = append(x, 1)
}

func main() {
	x := []int{0}
	fmt.Printf("Before: %v\n", x)
	update(x)
	fmt.Printf("After: %v\n", x)
}


