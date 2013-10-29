package main

import "fmt"

func update_map(x map[string](string)) {
	x["hello"]="world"
}

func main() {
	x := map[string](string){}
	fmt.Printf("Before: %v\n", x)
	update_map(x)
	fmt.Printf("After: %v\n", x)
}


