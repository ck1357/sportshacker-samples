/* 
 .. but you can also pass strings by reference
 */

package main

import "fmt"

func update(x *string) {
	*x+="!"
}

func main() {
	x := "Hello World" 
	fmt.Printf("Before: %v\n", x)
	update(&x)
	fmt.Printf("After: %v\n", x)
}


