package main

import "fmt"

func update_string(x *string) {
	*x+="!"
}

func main() {
	x := "Hello World" 
	fmt.Printf("Before: %v\n", x)
	update_string(&x)
	fmt.Printf("After: %v\n", x)
}


