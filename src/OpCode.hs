{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use :" #-}
module OpCode (
    buildIntructions
    , initContext
    , Context(funcs)
    , OpCode(PushVar, PushFunc, Call, Ret, JumpAddr, Pop, Exit, PushInt, CallBuiltIn)
) where

import qualified Desugarer as D
import qualified Data.Map as M
import Data.Function ((&))
import Data.Foldable (Foldable(toList))
import Control.Arrow ((>>>))

data OpCode
    = PushVar Int
    | PushFunc Int Int 
    | PushInt Int
    | Call Int
    | Ret Int
    | Pop
    | JumpAddr Int
    | Exit
    | CallBuiltIn Int String String
    deriving Show 

data Context = Context {
    vars :: M.Map String Int
    , funcs :: [[((Int,Int), OpCode)]] 
} deriving Show

initContext :: Context 
initContext = Context {
    vars = M.empty
    , funcs = []
}

buildIntruction :: Context -> D.NodeLLWithMetaData D.Context -> Either String (Context, [((Int,Int),OpCode)])
buildIntruction context (nodeContext, node, _) = case node of 
    D.Var varname -> maybe 
        (Left $ "At line " ++ show (nodeContext & D.linNum) ++ ", and column " ++ show (nodeContext & D.colNum) ++ " : variable " ++ show varname ++ "is not found")
        (\varRef->return (context, [
            (location, PushVar varRef)
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
                        (curContext, [(location, PushVar captureRef)] ++ instrs)
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
                                [(location, JumpAddr jumpAddr)] 
                                ++ funcInstrs 
                                ++ [(location, Ret $ count + 1)]
                            )
                            :(curContext & funcs) 
                        }, 
                        instrs ++ [(location, PushFunc (count - 1) jumpAddr)]
                        )
                )
    
            )
            

    D.Apply func' arg' -> do 
        (context', arg) <- buildIntruction context arg'
        (context'', func) <- buildIntruction context' func'
        return (context'', arg ++ func ++ [(location, Call jumpAddr), (location, JumpAddr jumpAddr)])
    D.Null node' -> buildIntruction context node'
    D.IntNodeLL int -> return (context, [(location, PushInt int)])
    D.RunBuiltInLL argc package symbol -> return  (context, [(location, CallBuiltIn argc package symbol)])
    where 
                jumpAddr = (+) <$> D.funcCount <*> D.funcCallCount $ nodeContext
                location = nodeContext & D.getLocation

buildIntructions :: Context -> [D.NodeLL D.Context] -> Either String (Context, [((Int,Int),OpCode)])
buildIntructions context [] = return (context,[((-1,-1), Exit)])
buildIntructions context (node':nodes) = case node' of 
    D.Null node -> do 
        let (nodellContext,_,_) = node
        let location = nodellContext & D.getLocation
        (context', instr) <- buildIntruction context node
        (context'', instrs) <- buildIntructions context' nodes
        return (context'', instr ++ [(location, Pop)] ++ instrs)
    _ -> Left $ "Invalid AST: " ++ show node'