module GoCodeGen (
    goCodeGen
    , goCodeGenBoilerPlate
) where
import OpCode (OpCode (JumpAddr, PushVar, Pop, Exit, PushFunc, Call, Ret, PushInt))
import Data.Functor ((<&>))
import Data.Function ((&))


goCodeGen :: OpCode -> String
goCodeGen (JumpAddr jumpAddr) = "JUMP_ADDR_"++show jumpAddr++":\n\
\\t\n"
goCodeGen (PushVar varPTR) = "\tstack_ptr += 1\n\
\\tstack[stack_ptr + scope_ptr] = stack[scope_ptr - " ++ show varPTR ++ "]\n"
goCodeGen (PushFunc captureNum funcPTR) = "\treg = WrappedFunction{\n\
\\t\tfunc_addr: " ++ show funcPTR ++ ",\n\
\\t\tcontext: []Wrapped{\n" ++ (((if captureNum <= 0 then [] else [0..captureNum - 1]) <&> (\capturePTR->"\
\\t\t\tstack[stack_ptr + scope_ptr - " ++ show capturePTR ++ "],\n")) & concat) ++ "\
\\t\t},\n\
\\t}\n\
\\tfor i := 0; i < " ++ show (captureNum - 1) ++ "; i += 1 {\n\
\\t\tstack[stack_ptr + scope_ptr - i] = nil\n\
\\t}\n\
\\tstack_ptr -= (" ++ show (captureNum - 1) ++ ")\n\
\\tstack[stack_ptr + scope_ptr] = reg\n"
goCodeGen (Call returnAddr) = "\
\\treg = stack[stack_ptr + scope_ptr]\n\
\\tstack[stack_ptr + scope_ptr] = nil\n\
\\tstack_ptr -= 1\n\
\\tswitch reg_unwrapped := reg.(type) {\n\
\\tcase WrappedFunction:\n\
\\t\tfor i := len(reg_unwrapped.context) - 1; i >= 0 ; i -= 1 {\n\
\\t\t\tstack_ptr += 1\n\
\\t\t\tstack[stack_ptr + scope_ptr] = reg_unwrapped.context[i]\n\
\\t\t}\n\
\\t\tstack_ptr += 1\n\
\\t\tstack[stack_ptr + scope_ptr] = WrappedInt{\n\
\\t\t\tnum: "++ show returnAddr ++ ",\n\
\\t\t}\n\
\\t\tstack[stack_ptr + scope_ptr + 1] = WrappedInt{\n\
\\t\t\tnum: scope_ptr,\n\
\\t\t}\n\
\\t\tjump_addr = reg_unwrapped.func_addr\n\
\\t\tscope_ptr = scope_ptr + stack_ptr\n\
\\t\tstack_ptr = 1\n\
\\tdefault:\n\
\\t\tfmt.Println(\"type error: \" + reg.show() + \" is not a function\")\n\
\\t\treturn\n\
\\t}\n\
\\tgoto JUMP\n"
goCodeGen (Ret argCount) = "\
\\tswitch ret_addr := stack[scope_ptr].(type) {\n\
\\tcase WrappedInt:\n\
\\t\tjump_addr = ret_addr.num\n\
\\tdefault:\n\
\\t\tfmt.Println(\"type error: \" + ret_addr.show() + \" is not an int\")\n\
\\t\treturn\n\
\\t}\n\
\\treg = stack[stack_ptr + scope_ptr]\n\
\\tswitch prev_scope_ptr := stack[scope_ptr + 1].(type) {\n\
\\tcase WrappedInt:\n\
\\t\tfor i := stack_ptr; i > -(" ++ show (argCount - 1) ++ "); i -= 1 {\n\
\\t\t\tstack[scope_ptr + i] = nil\n\
\\t\t}\n\
\\t\tstack_ptr = scope_ptr - prev_scope_ptr.num - "++ show (argCount - 1) ++"\n\
\\t\tscope_ptr = prev_scope_ptr.num\n\
\\t\tstack[stack_ptr + scope_ptr] = reg\n\
\\tdefault:\n\
\\t\tfmt.Println(\"type error\" + prev_scope_ptr.show() + \" is not an int\")\n\
\\t\treturn\n\
\\t}\n\
\\tgoto JUMP\n"
goCodeGen Pop = "\
\\tstack[stack_ptr + scope_ptr] = nil\n\
\\tstack_ptr -= 1\n"
goCodeGen Exit = "\treturn\n"
goCodeGen (PushInt int) = "\
\\tstack_ptr += 1\n\
\\tstack[scope_ptr + stack_ptr] = WrappedInt{\n\
\\t\tnum: " ++ show int ++ ",\n\
\\t}\n"
-- goCodeGen _ = ""

goCodeGenBoilerPlate :: Int -> String -> String
goCodeGenBoilerPlate jumpAddrNum code = "\

\package main\n\
\\n\
\import \"fmt\"\n\
\import \"strconv\"\n\
\\n\
\type Wrapped interface {\n\
\\tshow() string\n\
\}\n\
\\t\n\
\type WrappedFunction struct {\n\
\\tfunc_addr int\n\
\\tcontext []Wrapped\n\
\}\n\
\\n\
\func (_ WrappedFunction) show() string {\n\
\\treturn \"[function]\"\n\
\}\n\
\type WrappedInt struct {\n\
\\tnum int\n\
\}\n\
\\n\
\func (i WrappedInt) show() string {\n\
\\treturn strconv.FormatInt(int64(i.num), 10)\n\
\}\n\
\\n\
\func main() {\n\
\\n\
\\tvar jump_addr int = 0\n\
\\tvar stack []Wrapped = make([]Wrapped, 1000)\n\
\\tvar scope_ptr int = -1\n\
\\tvar stack_ptr int = 0\n\
\\tvar reg Wrapped\n\
\\n\
\JUMP:\n\
\\n\
\\tswitch jump_addr {\n"
    ++ concat ([0..jumpAddrNum] <&> \jumpAddr->"\
\\tcase " ++ show jumpAddr ++ ":\n\
\\t\tgoto JUMP_ADDR_" ++ show jumpAddr ++ "\n\n") ++ "\
\\t}\n\
\JUMP_ADDR_0:\n"
    ++ code ++ "\
\}\n"