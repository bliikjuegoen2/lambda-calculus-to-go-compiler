module GoCodeGen (
    goCodeGen
    , goCodeGenBoilerPlate
) where
import OpCode (OpCode (JumpAddr, PushVar, Pop, Exit, PushFunc, Call, Ret, PushInt, CallBuiltIn))
import Data.Functor ((<&>))
import Data.Function ((&))
import Control.Arrow ((>>>))


goCodeGen :: OpCode -> String
goCodeGen (JumpAddr jumpAddr) = "JUMP_ADDR_"++show jumpAddr++":\n\
\\t\n"
goCodeGen (PushVar varPTR) = "\tstack_ptr += 1\n\
\\tstack[stack_ptr + scope_ptr] = stack[scope_ptr - " ++ show varPTR ++ "]\n"
goCodeGen (PushFunc captureNum funcPTR) = "\
\\treg = runtime.MakeFunction(\n\
\\t\t" ++ show funcPTR ++ ",\n\
\\t\t[]runtime.Wrapped{\n" ++ (((if captureNum <= 0 then [] else [0..captureNum - 1]) <&> (\capturePTR->"\
\\t\t\tstack[stack_ptr + scope_ptr - " ++ show capturePTR ++ "],\n")) & concat) ++ "\
\\t\t},\n\
\\t)\n\
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
\\tcase runtime.WrappedFunction:\n\
\\t\tfor i := len(reg_unwrapped.Context()) - 1; i >= 0 ; i -= 1 {\n\
\\t\t\tstack_ptr += 1\n\
\\t\t\tstack[stack_ptr + scope_ptr] = reg_unwrapped.Context()[i]\n\
\\t\t}\n\
\\t\tstack_ptr += 1\n\
\\t\tstack[stack_ptr + scope_ptr] = runtime.MakeInt("++ show returnAddr ++ ")\n\
\\t\tstack[stack_ptr + scope_ptr + 1] = runtime.MakeInt(scope_ptr)\n\
\\t\tjump_addr = reg_unwrapped.FuncAddr()\n\
\\t\tscope_ptr = scope_ptr + stack_ptr\n\
\\t\tstack_ptr = 1\n\
\\tdefault:\n\
\\t\tfmt.Println(\"type error: \" + reg.Show() + \" is not a function\")\n\
\\t\treturn\n\
\\t}\n\
\\tgoto JUMP\n"
goCodeGen (Ret argCount) = "\
\\tswitch ret_addr := stack[scope_ptr].(type) {\n\
\\tcase runtime.WrappedInt:\n\
\\t\tjump_addr = ret_addr.GetInt()\n\
\\tdefault:\n\
\\t\tfmt.Println(\"type error: \" + ret_addr.Show() + \" is not an int\")\n\
\\t\treturn\n\
\\t}\n\
\\treg = stack[stack_ptr + scope_ptr]\n\
\\tswitch prev_scope_ptr := stack[scope_ptr + 1].(type) {\n\
\\tcase runtime.WrappedInt:\n\
\\t\tfor i := stack_ptr; i > -(" ++ show (argCount - 1) ++ "); i -= 1 {\n\
\\t\t\tstack[scope_ptr + i] = nil\n\
\\t\t}\n\
\\t\tstack_ptr = scope_ptr - prev_scope_ptr.GetInt() - "++ show (argCount - 1) ++"\n\
\\t\tscope_ptr = prev_scope_ptr.GetInt()\n\
\\t\tstack[stack_ptr + scope_ptr] = reg\n\
\\tdefault:\n\
\\t\tfmt.Println(\"type error\" + prev_scope_ptr.Show() + \" is not an int\")\n\
\\t\treturn\n\
\\t}\n\
\\tgoto JUMP\n"
goCodeGen Pop = "\
\\tstack[stack_ptr + scope_ptr] = nil\n\
\\tstack_ptr -= 1\n"
goCodeGen Exit = "\treturn\n"
goCodeGen (PushInt int) = "\
\\tstack_ptr += 1\n\
\\tstack[scope_ptr + stack_ptr] = runtime.MakeInt(" ++ show int ++ ")\n"
goCodeGen (CallBuiltIn argc package symbol) = "\
\\treg = " ++ package ++ "." ++ symbol ++ "(\n"
    ++ ([1..argc] <&> (show >>> ("\t\tstack[scope_ptr - " ++) >>> (++ "],\n")) & concat) ++ "\
\\t)\n\
\\tstack_ptr += 1\n\
\\tstack[scope_ptr + stack_ptr] = reg\n"
-- goCodeGen _ = ""

goCodeGenBoilerPlate :: Int -> String -> String
goCodeGenBoilerPlate jumpAddrNum code = "\

\package main\n\
\\n\
\import \"fmt\"\n\
\import \"github.com/bliikjuegoen2/lambda-calculus-to-go-compiler/runtime\"\n\
\\n\
\func main() {\n\
\\n\
\\tvar jump_addr int = 0\n\
\\tvar stack []runtime.Wrapped = make([]runtime.Wrapped, 1000)\n\
\\tvar scope_ptr int = -1\n\
\\tvar stack_ptr int = 0\n\
\\tvar reg runtime.Wrapped\n\
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