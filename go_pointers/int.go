package main

import "fmt"

func update_int(x int) {
	x+=1
}

func main() {
	x := 0
	fmt.Printf("Before: %v\n", x)
	update_int(x)
	fmt.Printf("After: %v\n", x)
}


