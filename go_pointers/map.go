/*
 maps are reference types
 */

package main

import "fmt"

func update(x map[string](string)) {
	x["hello"]="world"
}

func main() {
	x := map[string](string){}
	fmt.Printf("Before: %v\n", x)
	update(x)
	fmt.Printf("After: %v\n", x)
}


