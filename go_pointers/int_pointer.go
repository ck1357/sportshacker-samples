/*
 .. but you can also pass int by reference
 */

package main

import "fmt"

func update(x *int) {
	*x+=1
}

func main() {
	x := 0
	fmt.Printf("Before: %v\n", x)
	update(&x)
	fmt.Printf("After: %v\n", x)
}


