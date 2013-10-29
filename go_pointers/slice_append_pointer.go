/*

 https://groups.google.com/forum/#!msg/golang-nuts/QRkJIRIrVLM/QV9MbFJqkJkJ

  In reality the assertion that slices are passed as reference is a bit off. Everything is passed by value in Go, but a slice is actually a triplet structure. It contains a pointer to an array, a length and a capacity field. You can pass this directly without a pointer since it's not much overhead (1 pointer + 2 ints vs 1 pointer). The result is that if you modify just the array contents, your modifications will get reflected nicely (http://play.golang.org/p/ahXnRXLcrP). However, if you modify any of the defining triplets (ptr, len, cap) - which append does - you do need the slice pointer since you'll lose the modifications otherwise.
 
 */

package main

import "fmt"

func update_slice(x *[]int) {
	*x = append(*x, 1)
}

func main() {
	x := []int{0}
	fmt.Printf("Before: %v\n", x)
	update_slice(&x)
	fmt.Printf("After: %v\n", x)
}


