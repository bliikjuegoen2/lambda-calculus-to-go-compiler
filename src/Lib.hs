{-# LANGUAGE LambdaCase #-}
module Lib
    ( compiler
    ) where
import LineNumber (withLocation)
import Tokenizer (tokenizer)
import Operation (pipe, eitherOutput)
import Parser (parser)
import Data.Functor ((<&>))
import Desugarer (desugarer, cleanContext, scanTravL, NodeLL (Null), evalFuncCount, scanTravR, evalRefVars, evalVars, evalFuncCallCount, finalizeContext)
import Control.Arrow ((>>>), Arrow (first, second))
import qualified Data.Set as S
import qualified Data.Map as M
import OpCode (buildIntructions, initContext, Context (funcs), OpCode (JumpAddr))
import Data.Function ((&))
import Util (dup)
import Data.Foldable (Foldable(foldl'))
import GoCodeGen (goCodeGen, goCodeGenBoilerPlate)

compiler = withLocation id `pipe` tokenizer `pipe` parser 
    <&> (
        fmap (
        desugarer >>> Null >>> fmap cleanContext 
    >>> scanTravL evalFuncCount 0 >>> scanTravL evalFuncCallCount 0
    >>> scanTravR evalRefVars S.empty >>> scanTravL evalVars M.empty
    >>> fmap finalizeContext) 
    >>> buildIntructions initContext
    ) 
    & eitherOutput id (fmap (\(context, code)-> code ++ concat (funcs context)) 
    >>> fmap (dup >>> first (foldl' (\maxJumpAddr-> \case 
        JumpAddr jumpAddr -> maxJumpAddr `max` jumpAddr
        _ -> maxJumpAddr
    ) (0 :: Int)) 
    >>> second (>>= goCodeGen) >>> uncurry goCodeGenBoilerPlate
    )
    )

