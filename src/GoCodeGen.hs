module GoCodeGen (
    goCodeGen
    , goCodeGenBoilerPlate
) where
import OpCode (OpCode (JumpAddr, PushVar, Pop))
import Data.Functor ((<&>))


goCodeGen :: OpCode -> String
goCodeGen (JumpAddr jumpAddr) = "JUMP_ADDR_"++show jumpAddr++":\n\
\\t\n"
goCodeGen (PushVar varPTR) = "\tstack_ptr += 1\n\
\\tstack[ stack_ptr + scope_ptr ] = stack[ stack_ptr - " ++ show varPTR ++ " ]\n"
goCodeGen Pop = "\tstack_ptr -= 1\n"
goCodeGen _ = ""

goCodeGenBoilerPlate :: Int -> String -> String
goCodeGenBoilerPlate jumpAddrNum code = "\

\package main\n\
\\n\
\type WrappedFunction struct {\n\
\\tfunc_addr int\n\
\\tcontext []WrappedFunction\n\
\}\n\
\\n\
\func main() {\n\
\\n\
\\tvar jump_addr int = 0\n\
\\tvar stack []WrappedFunction = make([]WrappedFunction, 1000)\n\
\\tvar scope_ptr int = -1\n\
\\tvar stack_ptr int = 0\n\
\\n\
\\tswitch jump_addr {\n"
    ++ concat ([0..jumpAddrNum] <&> \jumpAddr->"\
\\tcase " ++ show jumpAddr ++ ":\n\
\\t\tgoto JUMP_ADDR_" ++ show jumpAddr ++ "\n\n") ++ "\
\\t}\n\
\JUMP_ADDR_0:\n"
    ++ code ++ "\
\}\n"