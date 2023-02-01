package stdlib

import (
	"fmt"

	"github.com/bliikjuegoen2/runtime/core"
)

func Print(x core.Wrapped) core.Wrapped {
	fmt.Println(x.Show())
	return core.MakeInt(0)
}

func Plus(a core.Wrapped, b core.Wrapped) core.Wrapped {
	var x int
	var y int
	switch unwrapped_a := a.(type) {
	case core.WrappedInt:
		x = unwrapped_a.GetInt()
	default:
		fmt.Println("type error")
		return core.MakeInt(0)
	}

	switch unwrapped_b := b.(type) {
	case core.WrappedInt:
		y = unwrapped_b.GetInt()
	default:
		fmt.Println("type error")
		return core.MakeInt(0)
	}

	return core.MakeInt(x + y)
}
