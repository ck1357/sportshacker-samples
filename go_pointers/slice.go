/*
 slices are reference types ..
 */

package main

import "fmt"

func update(x []int) {
	x[0]+=1
}

func main() {
	x := []int{0}
	fmt.Printf("Before: %v\n", x)
	update(x)
	fmt.Printf("After: %v\n", x)
}


