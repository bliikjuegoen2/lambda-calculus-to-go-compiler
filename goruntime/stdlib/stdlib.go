package stdlib

import (
	"fmt"

	"github.com/bliikjuegoen2/lambda-calculus-to-go-compiler/goruntime/core"
)

func Print(x core.Wrapped) core.Wrapped {
	fmt.Println(x.Show())
	return core.MakeInt(0)
}

func unwrap(a core.Wrapped, b core.Wrapped, x *int, y *int) bool {
	switch unwrapped_a := a.(type) {
	case core.WrappedInt:
		*x = unwrapped_a.GetInt()
	default:
		fmt.Println("type error")
		return true
	}

	switch unwrapped_b := b.(type) {
	case core.WrappedInt:
		*y = unwrapped_b.GetInt()
	default:
		fmt.Println("type error")
		return true
	}

	return false
}

func Plus(a core.Wrapped, b core.Wrapped) core.Wrapped {
	var x int
	var y int

	if unwrap(a, b, &x, &y) {
		return core.MakeInt(0)
	}

	return core.MakeInt(x + y)
}

func Minus(a core.Wrapped, b core.Wrapped) core.Wrapped {
	var x int
	var y int

	if unwrap(a, b, &x, &y) {
		return core.MakeInt(0)
	}

	return core.MakeInt(x - y)
}

func Multiply(a core.Wrapped, b core.Wrapped) core.Wrapped {
	var x int
	var y int

	if unwrap(a, b, &x, &y) {
		return core.MakeInt(0)
	}

	return core.MakeInt(x * y)
}

func Divide(a core.Wrapped, b core.Wrapped) core.Wrapped {
	var x int
	var y int

	if unwrap(a, b, &x, &y) {
		return core.MakeInt(0)
	}

	return core.MakeInt(x / y)
}

func Equal(a core.Wrapped, b core.Wrapped, ifTrue core.Wrapped, ifFalse core.Wrapped) core.Wrapped {
	var x int
	var y int

	if unwrap(a, b, &x, &y) {
		return core.MakeInt(0)
	}

	if x == y {
		return ifTrue
	} else {
		return ifFalse
	}
}

func LessThan(a core.Wrapped, b core.Wrapped, ifTrue core.Wrapped, ifFalse core.Wrapped) core.Wrapped {
	var x int
	var y int

	if unwrap(a, b, &x, &y) {
		return core.MakeInt(0)
	}

	if x < y {
		return ifTrue
	} else {
		return ifFalse
	}
}

func GreaterThan(a core.Wrapped, b core.Wrapped, ifTrue core.Wrapped, ifFalse core.Wrapped) core.Wrapped {
	var x int
	var y int

	if unwrap(a, b, &x, &y) {
		return core.MakeInt(0)
	}

	if x > y {
		return ifTrue
	} else {
		return ifFalse
	}
}

func Input(_ core.Wrapped) core.Wrapped {
	var x int
	fmt.Scanf("%d", &x)
	return core.MakeInt(x)
}
