package main

import "fmt"

func update_slice(x []int) {
	x[0]+=1
}

func main() {
	x := []int{0}
	fmt.Printf("Before: %v\n", x)
	update_slice(x)
	fmt.Printf("After: %v\n", x)
}


