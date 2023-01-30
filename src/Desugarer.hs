module Desugarer (
    NodeLL(Null)
    , Context
    , desugarer
    , cleanContext
    , evalFuncCount
    , evalRefVars
    , evalVars
    , finalizeContext
) where
import Parser (NodeWithMetaData, Node (Variable, Function, Call, VariableDef))
import Data.Function ((&))
import Control.Arrow (second, (>>>))
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Bool.HT (if')

data NodeLL metaData
    = Var String 
    | Func String (NodeLLWithMetaData metaData)
    | Apply (NodeLLWithMetaData metaData) (NodeLLWithMetaData metaData)
    | Null (NodeLLWithMetaData metaData)
    deriving Show

type NodeLLWithMetaData metaData = (metaData, NodeLL metaData, metaData)

instance Functor NodeLL where
    fmap f node = case node of 
        Var varname -> Var varname 
        Func arg (contextL, x, contextR) -> Func arg (f contextL, f <$> x, f contextR) 
        Apply (funcContextL, func, funcContextR) (argContextL, arg, argContextR) 
            -> Apply (f funcContextL, f <$> func, f funcContextR) (f argContextL, f <$> arg, f argContextR)
        Null (contextL, x, contextR) -> Null (f contextL, f <$> x, f contextR)

instance Foldable NodeLL where 
    foldMap f node = case node of
        Var _ -> mempty
        Func _ (contextL, x, contextR) -> f contextL <> f `foldMap` x <> f contextR
        Apply (funcContextL, func, funcContextR) (argContextL, arg, argContextR) 
            -> f funcContextL <> f `foldMap` func <> f funcContextR <> f argContextL <> f `foldMap` arg <> f argContextR
        Null (contextL, x, contextR) -> f contextL <> f `foldMap` x <> f contextR

instance Traversable NodeLL where 
    traverse f node = case node of 
        Var varname -> pure $ Var varname 
        Func arg (contextL, x, contextR) -> Func arg <$> ((,,) <$> f contextL <*> f `traverse` x <*> f contextR) 
        Apply (funcContextL, func, funcContextR) (argContextL, arg, argContextR) 
            -> Apply <$> ((,,) <$> f funcContextL <*> f `traverse` func <*> f funcContextR) <*> ((,,) <$> f argContextL <*> f `traverse` arg <*> f argContextR)
        Null (contextL, x, contextR) -> Null <$> ((,,) <$> f contextL <*> f `traverse` x <*> f contextR)

data Context = Context {
    linNum :: Int 
    , colNum :: Int
    , funcCount :: Int
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
    , refVars = S.empty
    , variables = M.empty 
}

data ContextAction = ContextAction {
    isIncrementingFuncCount :: Bool
    , refVarsAction :: RefVarsAction
    , variablesAction :: VariablesAction
}
    -- deriving Show

instance Show ContextAction where 
    show = const ""

data RefVarsAction 
    = NoRefVarsAction 
    | AddRefVar String 
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

desugarer :: Desugarer 
desugarer (context, node) = case node of 
    Variable varname -> buildVars varname context
    Function args body -> foldr 
        (\arg innerFunc-> buildFunc arg innerFunc context) 
        (desugarer body) 
        args
    Call func args -> foldl 
        (\intermediate arg-> Apply intermediate arg & withContext context) 
        (desugarer func)
        (desugarer <$> args)
    VariableDef defs expr -> foldr 
        (\(varname, definition) innerExpr -> Apply (buildFunc varname innerExpr context) definition & withContext context) 
        (desugarer expr) 
        (second desugarer <$> defs)

cleanContext :: ((Int, Int), ContextAction, Context) -> (ContextAction, Context)
cleanContext ((linNum, colNum), action, context) = (action, context {
    linNum = linNum
    , colNum = colNum
})

evalFuncCount :: Int -> (ContextAction, Context) -> (Int, (ContextAction, Context))
evalFuncCount curFuncCount (action, context) = if action & isIncrementingFuncCount 
    then ret $ curFuncCount + 1 
    else ret curFuncCount
    where 
        ret newFuncCount = (newFuncCount, (action, context {funcCount = newFuncCount}))

evalRefVars :: S.Set String -> (ContextAction, Context) -> (S.Set String, (ContextAction, Context))
evalRefVars curRefVars (action, context) = case action & refVarsAction of
    NoRefVarsAction -> ret curRefVars
    AddRefVar varname -> ret $ curRefVars & S.insert varname
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