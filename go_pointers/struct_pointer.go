package main

import "fmt"

type MyStruct struct {
	MyInt int
}

func update_struct(x *MyStruct) {
	x.MyInt=1 /* why not  *x.MyInt=1 ? */
}

func main() {
	x := MyStruct{
	MyInt: 0,
	}
	fmt.Printf("Before: %v\n", x)
	update_struct(&x)
	fmt.Printf("After: %v\n", x)
}


