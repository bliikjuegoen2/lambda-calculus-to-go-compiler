{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use :" #-}
module OpCode (
    buildIntructions
    , initContext
    , Context(funcs)
    , OpCode(PushVar, PushFunc, Call, Ret, JumpAddr, Pop, Exit)
) where

import qualified Desugarer as D
import qualified Data.Map as M
import Data.Function ((&))
import Data.Foldable (Foldable(toList))
import Control.Arrow ((>>>))

data OpCode
    = PushVar Int
    | PushFunc Int Int 
    | Call Int
    | Ret Int
    | Pop
    | JumpAddr Int
    | Exit
    deriving Show 

data Context = Context {
    vars :: M.Map String Int
    , funcs :: [[OpCode]] 
} deriving Show

initContext :: Context 
initContext = Context {
    vars = M.empty
    , funcs = []
}

buildIntruction :: Context -> D.NodeLLWithMetaData D.Context -> Either String (Context, [OpCode])
buildIntruction context (nodeContext, node, _) = case node of 
    D.Var varname -> maybe 
        (Left $ "At line " ++ show (nodeContext & D.linNum) ++ ", and column " ++ show (nodeContext & D.colNum) ++ " : variable " ++ show varname ++ "is not found")
        (\varRef->return (context, [
            PushVar varRef
        ])) 
        $ context & vars & M.lookup varname
    D.Func arg body -> traverse (\captureVar-> maybe 
                (Left $ "At line " ++ show (nodeContext & D.linNum) ++ ", and column " ++ show (nodeContext & D.colNum) ++ " : variable " ++ show captureVar ++ "is not found") 
                (\captureRef-> return (captureVar, captureRef)) 
                $ context & vars & M.lookup captureVar
                ) 
            -- get all the variables that would be caputured in the lambda
                (nodeContext & D.refVars & toList)
            >>= (
                foldr 
                (\(captureVar, captureRef) (count, captureVars, (curContext, instrs)) -> 
                    (
                        count + 1, 
                        captureVars & M.insert captureVar count, 
                        (curContext, [PushVar captureRef] ++ instrs)
                    )
                ) 
                (1 :: Int, M.empty, (context, []))
            >>> (\(count, captureVars, (curContext', instrs))-> do 
                    (curContext, funcInstrs) <- buildIntruction 
                        curContext' { vars = captureVars & M.insert arg count } 
                        body 
                    return (
                        curContext' { 
                            funcs = ( 
                                [JumpAddr jumpAddr] 
                                ++ funcInstrs 
                                ++ [Ret $ count + 1]
                            )
                            :(curContext & funcs) 
                        }, 
                        instrs ++ [PushFunc (count - 1) jumpAddr]
                        )
                )
            )
            

    D.Apply func' arg' -> do 
        (context', arg) <- buildIntruction context arg'
        (context'', func) <- buildIntruction context' func'
        return (context'', arg ++ func ++ [Call jumpAddr, JumpAddr jumpAddr])
    D.Null node' -> buildIntruction context node'
    where 
                jumpAddr = (+) <$> D.funcCount <*> D.funcCallCount $ nodeContext

buildIntructions :: Context -> [D.NodeLL D.Context] -> Either String (Context, [OpCode])
buildIntructions context [] = return (context,[Exit])
buildIntructions context (node':nodes) = case node' of 
    D.Null node -> do 
        (context', instr) <- buildIntruction context node
        (context'', instrs) <- buildIntructions context' nodes
        return (context'', instr ++ [Pop] ++ instrs)
    _ -> Left $ "Invalid AST: " ++ show node'