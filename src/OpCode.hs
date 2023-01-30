{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use :" #-}
module OpCode (
    buildIntructions
    , initContext
) where

import qualified Desugarer as D
import Control.Arrow ((>>>), Arrow (second))
import Control.Monad (join, (>=>))
import qualified Data.Map as M
import Data.Function ((&))
import Data.Foldable (Foldable(toList))
import Data.Functor ((<&>))

data OpCode
    = PushVar Int
    | PushFunc Int 
    | Call 
    | Ret 
    | Pop
    deriving Show 

data Context = Context {
    vars :: M.Map String Int
    , funcs :: [(Int, [OpCode])] -- the function number must be stored because the ordering of the functions might get messed up
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
                (0 :: Int, M.empty, (context, [PushFunc $ D.funcCount nodeContext - 1]))
            >>> (\(count, captureVars, (curContext', instrs))-> do 
                    (curContext, funcInstrs) <- buildIntruction 
                        curContext' { vars = captureVars & M.insert arg count } 
                        body 
                    return (curContext' { funcs = (D.funcCount nodeContext - 1, funcInstrs ++ [Ret]):(curContext & funcs) }, instrs)
                )
            )

    D.Apply func' arg' -> do 
        (context', arg) <- buildIntruction context arg'
        (context'', func) <- buildIntruction context' func'
        return (context'', arg ++ func ++ [Call])
    D.Null node' -> buildIntruction context node'

buildIntructions :: Context -> [D.NodeLL D.Context] -> Either String (Context, [OpCode])
buildIntructions context [] = return (context,[])
buildIntructions context (node':nodes) = case node' of 
    D.Null node -> do 
        (context', instr) <- buildIntruction context node
        (context'', instrs) <- buildIntructions context' nodes
        return (context'', instr ++ [Pop] ++ instrs)
    _ -> Left $ "Invalid AST: " ++ show node'