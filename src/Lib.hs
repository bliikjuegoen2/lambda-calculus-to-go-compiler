module Lib
    ( compiler
    ) where
import LineNumber (withLocation)
import Tokenizer (tokenizer)
import Operation (pipe, eitherOutput)
import Parser (parser)
import Data.Functor ((<&>))
import Desugarer (desugarer, cleanContext, scanTravL, NodeLL (Null), evalFuncCount, scanTravR, evalRefVars, evalVars, evalFuncCallCount, finalizeContext)
import Control.Arrow ((>>>))
import qualified Data.Set as S
import qualified Data.Map as M
import OpCode (buildIntructions, initContext)
import Data.Function ((&))

compiler = withLocation id `pipe` tokenizer `pipe` parser 
    <&> (fmap (desugarer >>> Null >>> fmap cleanContext 
    >>> scanTravL evalFuncCount 0 >>> scanTravL evalFuncCallCount 0
    >>> scanTravR evalRefVars S.empty >>> scanTravL evalVars M.empty
    >>> fmap finalizeContext) 
    >>> buildIntructions initContext
    ) 
    & eitherOutput id id
