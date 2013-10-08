/*
 .. but you can also pass by reference
 */

package main

import "fmt"

type MyStruct struct {
	MyInt int
}

func update(x *MyStruct) {
	x.MyInt=1 // not *x.MyInt=1
}

func main() {
	x := MyStruct{
	MyInt: 0,
	}
	fmt.Printf("Before: %v\n", x)
	update(&x)
	fmt.Printf("After: %v\n", x)
}


