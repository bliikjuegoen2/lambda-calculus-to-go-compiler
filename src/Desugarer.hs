{-# LANGUAGE TupleSections #-}
module Desugarer (
    NodeLL(Var, Func, Apply, Null, IntNodeLL, RunBuiltInLL)
    , NodeLLWithMetaData
    , Context(linNum, colNum, refVars, variables, funcCount, funcCallCount)
    , desugarer
    , cleanContext
    , evalFuncCount
    , evalFuncCallCount
    , evalRefVars
    , evalVars
    , finalizeContext
    , scanTravL
    , scanTravR
    , getLocation
) where
import Parser (NodeWithMetaData, Node (Variable, Function, Call, VariableDef, IntNode, IfStatement, Closure, EvalClosure, BuiltIn, RunBuiltIn, Block, RecursiveFunc))
import Data.Function ((&))
import Control.Arrow (second, (>>>), Arrow ((***), (&&&)))
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Bool.HT (if')
import Data.List (mapAccumL, mapAccumR)
import Data.Functor ((<&>))

data NodeLL metaData
    = Var String 
    | IntNodeLL Int
    | Func String (NodeLLWithMetaData metaData)
    | Apply (NodeLLWithMetaData metaData) (NodeLLWithMetaData metaData)
    | Null (NodeLLWithMetaData metaData)
    | RunBuiltInLL Int String String
    deriving Show

type NodeLLWithMetaData metaData = (metaData, NodeLL metaData, metaData)

instance Functor NodeLL where
    fmap f node = case node of 
        Var varname -> Var varname 
        Func arg (contextL, x, contextR) -> Func arg (f contextL, f <$> x, f contextR) 
        Apply (funcContextL, func, funcContextR) (argContextL, arg, argContextR) 
            -> Apply (f funcContextL, f <$> func, f funcContextR) (f argContextL, f <$> arg, f argContextR)
        Null (contextL, x, contextR) -> Null (f contextL, f <$> x, f contextR)
        IntNodeLL int -> IntNodeLL int
        RunBuiltInLL argc package symbol -> RunBuiltInLL argc package symbol

instance Foldable NodeLL where 
    foldMap f node = case node of
        Var _ -> mempty
        Func _ (contextL, x, contextR) -> f contextL <> f `foldMap` x <> f contextR
        Apply (funcContextL, func, funcContextR) (argContextL, arg, argContextR) 
            -> f funcContextL <> f `foldMap` func <> f funcContextR <> f argContextL <> f `foldMap` arg <> f argContextR
        Null (contextL, x, contextR) -> f contextL <> f `foldMap` x <> f contextR
        IntNodeLL _ -> mempty
        RunBuiltInLL {} -> mempty

instance Traversable NodeLL where 
    traverse f node = case node of 
        Var varname -> pure $ Var varname 
        Func arg (contextL, x, contextR) -> Func arg <$> ((,,) <$> f contextL <*> f `traverse` x <*> f contextR) 
        Apply (funcContextL, func, funcContextR) (argContextL, arg, argContextR) 
            -> Apply <$> ((,,) <$> f funcContextL <*> f `traverse` func <*> f funcContextR) <*> ((,,) <$> f argContextL <*> f `traverse` arg <*> f argContextR)
        Null (contextL, x, contextR) -> Null <$> ((,,) <$> f contextL <*> f `traverse` x <*> f contextR)
        IntNodeLL int -> pure $ IntNodeLL int
        RunBuiltInLL argc package symbol -> pure $ RunBuiltInLL argc package symbol

data Context = Context {
    linNum :: Int 
    , colNum :: Int
    , funcCount :: Int
    , funcCallCount :: Int
    , refVars :: S.Set String -- the difference between variables and refVars is that variables reprensents all the variables in the context while refVars just represent the referenced ones i.e variables >= refVars
    , variables :: M.Map String Int
}
    deriving Show

-- instance Show Context where 
--     show = variables >>> show

initContext :: Context
initContext = Context {
    linNum = 0
    , colNum = 0
    , funcCount = 0
    , funcCallCount = 0
    , refVars = S.empty
    , variables = M.empty 
}

getLocation :: Context -> (Int,Int) 
getLocation = linNum &&& colNum

data ContextAction = ContextAction {
    isIncrementingFuncCount :: Bool
    , isIncrementingFuncCallCount :: Bool
    , refVarsAction :: RefVarsAction
    , variablesAction :: VariablesAction
}
    -- deriving Show

instance Show ContextAction where 
    show = const ""

data RefVarsAction 
    = NoRefVarsAction 
    | AddRefVar String 
    | AddRefVars [String]
    | RemoveRefVar String
    deriving Show

data VariablesAction
    = NoVariablesAction 
    | AddVar String 
    | RemoveVar String 
    deriving Show

noAction :: ContextAction
noAction = ContextAction {
    isIncrementingFuncCount = False
    , isIncrementingFuncCallCount = False
    , refVarsAction = NoRefVarsAction
    , variablesAction = NoVariablesAction
}

type Desugarer = NodeWithMetaData -> NodeLLWithMetaData ((Int, Int), ContextAction, Context)

withAction :: (Int, Int) -> ContextAction -> ContextAction -> NodeLL ((Int, Int), ContextAction, Context) -> NodeLLWithMetaData ((Int, Int), ContextAction, Context)
withAction context actionL actionR node = (metaDataL, node, metaDataR)
    where 
        metaDataL = (context, actionL, initContext)
        metaDataR = (context, actionR, initContext)

withActionL :: (Int, Int) -> ContextAction -> NodeLL ((Int, Int), ContextAction, Context) -> NodeLLWithMetaData ((Int, Int), ContextAction, Context)
withActionL context action = withAction context action noAction

withActionR :: (Int, Int) -> ContextAction -> NodeLL ((Int, Int), ContextAction, Context) -> NodeLLWithMetaData ((Int, Int), ContextAction, Context)
withActionR context = withAction context noAction

withContext :: (Int, Int) -> NodeLL ((Int, Int), ContextAction, Context) -> NodeLLWithMetaData ((Int, Int), ContextAction, Context)
withContext context = withAction context noAction noAction

buildVars :: String -> (Int, Int) -> NodeLLWithMetaData ((Int, Int), ContextAction, Context)
buildVars varname context = Var varname & withActionR context noAction 
    { refVarsAction = AddRefVar varname }

buildFunc :: String -> NodeLLWithMetaData ((Int, Int), ContextAction, Context) -> (Int, Int) -> NodeLLWithMetaData ((Int, Int), ContextAction, Context)
buildFunc arg body context = Func arg body & withAction context  
    noAction { isIncrementingFuncCount = True, refVarsAction = RemoveRefVar arg, variablesAction = AddVar arg }
    noAction { variablesAction = RemoveVar arg}

buildApply :: NodeLLWithMetaData ((Int, Int), ContextAction, Context) -> NodeLLWithMetaData ((Int, Int), ContextAction, Context) -> (Int, Int) -> NodeLLWithMetaData ((Int, Int), ContextAction, Context)
buildApply func arg context = Apply func arg & withActionL context 
    noAction { isIncrementingFuncCallCount = True }

desugarer :: Desugarer 
desugarer (context, node) = case node of 
    Variable varname -> buildVars varname context
    Function args body -> foldr 
        (\arg innerFunc-> buildFunc arg innerFunc context) 
        (desugarer body) 
        args
    Call func args -> foldl 
        (\intermediate arg-> buildApply intermediate arg context) 
        (desugarer func)
        (desugarer <$> args)
    VariableDef defs expr -> foldr 
        (\(varname, definition) innerExpr -> buildApply (buildFunc varname innerExpr context) definition context) 
        (desugarer expr) 
        (second desugarer <$> defs)
    IntNode int -> IntNodeLL int & withContext context
    IfStatement cond ifTrue ifFalse -> 
        desugarer (context, EvalClosure (context, Call (context, Variable "if'") [cond, ifTrue, ifFalse]))
    Closure expr -> 
        desugarer (context, Function ["$"] expr)
    EvalClosure expr ->
        desugarer (context, Call expr [(context, IntNode 0)])
    BuiltIn argc package symbol -> desugarer (context, Function ([1..argc] <&> show) (context, RunBuiltIn argc package symbol))
    RunBuiltIn argc package symbol -> RunBuiltInLL argc package symbol & withActionR context 
        noAction { refVarsAction = [1..argc] <&> show & AddRefVars}
    Block code -> desugarer (context, VariableDef (code <&> ("_",)) (context, IntNode 0))
    RecursiveFunc args body -> desugarer (context, Call (context, Variable "Y'") [(context, Function ("rec":args) body)]) 

cleanContext :: ((Int, Int), ContextAction, Context) -> (ContextAction, Context)
cleanContext ((linNum, colNum), action, context) = (action, context {
    linNum = linNum
    , colNum = colNum
})

scanTravL :: Traversable t => (s -> a -> (s, b)) -> s -> t a -> t b
scanTravL f acc = mapAccumL f acc >>> snd

scanTravR :: Traversable t => (s -> a -> (s, b)) -> s -> t a -> t b
scanTravR f acc = mapAccumR f acc >>> snd

evalFuncCount :: Int -> (ContextAction, Context) -> (Int, (ContextAction, Context))
evalFuncCount curFuncCount (action, context) = if action & isIncrementingFuncCount 
    then ret $ curFuncCount + 1 
    else ret curFuncCount
    where 
        ret newFuncCount = (newFuncCount, (action, context {funcCount = newFuncCount}))

evalFuncCallCount :: Int -> (ContextAction, Context) -> (Int, (ContextAction, Context))
evalFuncCallCount curFuncCallCount (action, context) = if action & isIncrementingFuncCallCount 
    then ret $ curFuncCallCount + 1 
    else ret curFuncCallCount
    where 
        ret newFuncCallCount = (newFuncCallCount, (action, context {funcCallCount = newFuncCallCount}))

evalRefVars :: S.Set String -> (ContextAction, Context) -> (S.Set String, (ContextAction, Context))
evalRefVars curRefVars (action, context) = case action & refVarsAction of
    NoRefVarsAction -> ret curRefVars
    AddRefVar varname -> ret $ curRefVars & S.insert varname
    AddRefVars varnames -> ret $ curRefVars `S.union` S.fromList varnames
    RemoveRefVar varname -> ret $ curRefVars & S.delete varname
    where 
        ret newRefVars = (newRefVars, (action, context { refVars = newRefVars}))

evalVars :: M.Map String Int -> (ContextAction, Context) -> (M.Map String Int, (ContextAction, Context))
evalVars curVars (action, context) = case action & variablesAction of 
    NoVariablesAction -> ret curVars
    AddVar varname -> ret $ if curVars & M.member varname
        then curVars & M.update ((+ 1) >>> Just) varname 
        else curVars & M.insert varname 1
    RemoveVar varname -> ret $ curVars & M.update (minusOne >>> if' <$> (<= 0) <*> const Nothing <*> Just) varname
    where 
        ret newVariables = (newVariables, (action, context { variables = newVariables}))
        minusOne x = x - 1

finalizeContext :: (ContextAction, Context) -> Context
finalizeContext = snd