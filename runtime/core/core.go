package core

import (
	"strconv"
)

type Wrapped interface {
	Show() string
}

type WrappedFunction struct {
	func_addr int
	context   []Wrapped
}

func MakeFunction(func_addr int, context []Wrapped) WrappedFunction {
	return WrappedFunction{
		func_addr: func_addr,
		context:   context,
	}
}

func (_ WrappedFunction) Show() string {
	return "[function]"
}

func (fun WrappedFunction) Context() []Wrapped {
	return fun.context
}

func (fun WrappedFunction) FuncAddr() int {
	return fun.func_addr
}

type WrappedInt struct {
	num int
}

func MakeInt(num int) WrappedInt {
	return WrappedInt{
		num: num,
	}
}

func (num WrappedInt) GetInt() int {
	return num.num
}

func (i WrappedInt) Show() string {
	return strconv.FormatInt(int64(i.num), 10)
}
