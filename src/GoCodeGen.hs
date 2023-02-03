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
goCodeGen (PushVar varPTR) = "\tcpu.StackPtr += 1\n\
\\tcpu.Stack[cpu.ScopePtr + cpu.StackPtr] = cpu.Stack[cpu.ScopePtr - " ++ show varPTR ++ "]\n"
goCodeGen (PushFunc captureNum funcPTR) = "\
\\tcpu.Reg = core.MakeFunction(\n\
\\t\t" ++ show funcPTR ++ ",\n\
\\t\t[]core.Wrapped{\n" ++ (((if captureNum <= 0 then [] else [0..captureNum - 1]) <&> (\capturePTR->"\
\\t\t\tcpu.Stack[cpu.ScopePtr + cpu.StackPtr - " ++ show capturePTR ++ "],\n")) & concat) ++ "\
\\t\t},\n\
\\t)\n\
\\tfor i := 0; i < " ++ show (captureNum - 1) ++ "; i += 1 {\n\
\\t\tcpu.Stack[cpu.ScopePtr + cpu.StackPtr - i] = nil\n\
\\t}\n\
\\tcpu.StackPtr -= (" ++ show (captureNum - 1) ++ ")\n\
\\tcpu.Stack[cpu.ScopePtr + cpu.StackPtr] = cpu.Reg\n"
goCodeGen (Call returnAddr) = "\
\\tcpu.Reg = cpu.Stack[cpu.ScopePtr + cpu.StackPtr]\n\
\\tcpu.Stack[cpu.ScopePtr + cpu.StackPtr] = nil\n\
\\tcpu.StackPtr -= 1\n\
\\tswitch reg_unwrapped := cpu.Reg.(type) {\n\
\\tcase core.WrappedFunction:\n\
\\t\tfor i := len(reg_unwrapped.Context()) - 1; i >= 0 ; i -= 1 {\n\
\\t\t\tcpu.StackPtr += 1\n\
\\t\t\tcpu.Stack[cpu.ScopePtr + cpu.StackPtr] = reg_unwrapped.Context()[i]\n\
\\t\t}\n\
\\t\tcpu.StackPtr += 1\n\
\\t\tcpu.Stack[cpu.ScopePtr + cpu.StackPtr] = core.MakeInt("++ show returnAddr ++ ")\n\
\\t\tcpu.Stack[cpu.ScopePtr + cpu.StackPtr + 1] = core.MakeInt(cpu.ScopePtr)\n\
\\t\tcpu.JumpAddr = reg_unwrapped.FuncAddr()\n\
\\t\tcpu.ScopePtr = cpu.ScopePtr + cpu.StackPtr\n\
\\t\tcpu.StackPtr = 1\n\
\\tdefault:\n\
\\t\tfmt.Println(\"type error: \" + cpu.Reg.Show() + \" is not a function; return addr - " ++ show returnAddr ++ "\")\n\
\\t\treturn\n\
\\t}\n\
\\tgoto JUMP\n"
goCodeGen (Ret argCount) = "\
\\tswitch ret_addr := cpu.Stack[cpu.ScopePtr].(type) {\n\
\\tcase core.WrappedInt:\n\
\\t\tcpu.JumpAddr = ret_addr.GetInt()\n\
\\tdefault:\n\
\\t\tfmt.Println(\"type error: \" + ret_addr.Show() + \" is not an int\")\n\
\\t\treturn\n\
\\t}\n\
\\tcpu.Reg = cpu.Stack[cpu.StackPtr + cpu.ScopePtr]\n\
\\tswitch prev_scope_ptr := cpu.Stack[cpu.ScopePtr + 1].(type) {\n\
\\tcase core.WrappedInt:\n\
\\t\tfor i := cpu.StackPtr; i > -(" ++ show (argCount - 1) ++ "); i -= 1 {\n\
\\t\t\tcpu.Stack[cpu.ScopePtr + i] = nil\n\
\\t\t}\n\
\\t\tcpu.StackPtr = cpu.ScopePtr - prev_scope_ptr.GetInt() - "++ show (argCount - 1) ++"\n\
\\t\tcpu.ScopePtr = prev_scope_ptr.GetInt()\n\
\\t\tcpu.Stack[cpu.ScopePtr + cpu.StackPtr] = cpu.Reg\n\
\\tdefault:\n\
\\t\tfmt.Println(\"type error\" + prev_scope_ptr.Show() + \" is not an int\")\n\
\\t\treturn\n\
\\t}\n\
\\tgoto JUMP\n"
goCodeGen Pop = "\
\\tcpu.Stack[cpu.ScopePtr + cpu.StackPtr] = nil\n\
\\tcpu.StackPtr -= 1\n"
goCodeGen Exit = "\treturn\n"
goCodeGen (PushInt int) = "\
\\tcpu.StackPtr += 1\n\
\\tcpu.Stack[cpu.ScopePtr + cpu.StackPtr] = core.MakeInt(" ++ show int ++ ")\n"
goCodeGen (CallBuiltIn argc package symbol) = "\
\\tcpu.Reg = " ++ package ++ "." ++ symbol ++ "(\n"
    ++ ([(1-argc)..(-1)] <&> (show >>> ("\t\tcpu.Stack[cpu.ScopePtr + (" ++) >>> (++ ")],\n")) & concat) ++ "\
\\t\tcpu.Stack[cpu.ScopePtr - "++ show argc ++"],\n\
\\t)\n\
\\tcpu.StackPtr += 1\n\
\\tcpu.Stack[cpu.ScopePtr + cpu.StackPtr] = cpu.Reg\n"
-- goCodeGen _ = ""

goCodeGenBoilerPlate :: Int -> String -> String
goCodeGenBoilerPlate jumpAddrNum code = "\

\package main\n\
\\n\
\import \"fmt\"\n\
\import \"github.com/bliikjuegoen2/lambda-calculus-to-go-compiler/goruntime/core\"\n\
\import \"github.com/bliikjuegoen2/lambda-calculus-to-go-compiler/goruntime/stdlib\"\n\
\\n\
\func program(cpu *core.CPU){\n\
\\n\
\JUMP:\n\
\\n\
\\tswitch cpu.JumpAddr {\n"
    ++ concat ([0..jumpAddrNum] <&> \jumpAddr->"\
\\tcase " ++ show jumpAddr ++ ":\n\
\\t\tgoto JUMP_ADDR_" ++ show jumpAddr ++ "\n\n") ++ "\
\\t}\n\
\JUMP_ADDR_0:\n"
    ++ code ++ "\
\}\n\
\\n\
\func main() {\n\
\\tvar cpu core.CPU = core.MakeCPU()\n\
\\tprogram(&cpu)\n\
\}\n"