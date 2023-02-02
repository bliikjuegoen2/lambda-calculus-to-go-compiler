{-# LANGUAGE LambdaCase #-}
module Parser (
    Node(Variable, Function, Call, VariableDef, IntNode, IfStatement, Closure, EvalClosure, 
        BuiltIn, RunBuiltIn, Block, RecursiveFunc, Infixes)
    , NodeWithMetaData
    , parser
    , parseExpr
    , parseRecursiveFunc
) where
    
import Operation (Operation, eitherOutput, idOperation, filterOutput)
import Tokenizer (Token (ParenL, ParenR, Identifier, LambdaL, LambdaR, Set, In, Sep, IntLiteral, IF, THEN, ELSE, END, CLOSURE, RUNCLOSURE, BUILTIN, BLOCK, INFIXL, INFIXR))
import Control.Applicative (many, asum, Alternative (some))
import CharOperation (filterChar)
import Data.Function ((&))
import Control.Arrow ((>>>), Arrow (first, second))
import Util (dupFirst, mix)
import Data.Functor ((<&>))

data Node
    = Variable String
    | Function [String] NodeWithMetaData
    | Call NodeWithMetaData [NodeWithMetaData]
    | VariableDef [(String, NodeWithMetaData)] NodeWithMetaData
    | IntNode Int
    | IfStatement NodeWithMetaData NodeWithMetaData NodeWithMetaData
    | Closure NodeWithMetaData
    | EvalClosure NodeWithMetaData
    | BuiltIn Int String String 
    | RunBuiltIn Int String String -- used for desugaring - parser shouldn't generate this node
    | Block [NodeWithMetaData]
    | RecursiveFunc [String] NodeWithMetaData
    | Infixes NodeWithMetaData [(NodeWithMetaData,NodeWithMetaData)]

instance Show Node where 
    show (Variable var) = show ("var", var)
    show (Function args (_, body)) = show ("func", args, body)
    show (Call (_, func) args') = let args = fmap snd args' in show ("call", func, args)
    show (VariableDef vars' (_, body)) = let var = second snd <$> vars' in show ("def", var, body)
    show (IntNode int) = show ("int", int)
    show (IfStatement cond ifTrue ifFalse) = show ("if", cond, "then", ifTrue, "else", ifFalse)
    show (Closure expr) = show ("[",expr,"]")
    show (EvalClosure expr) = show ("*", expr)
    show (BuiltIn package argc symbol) = show("builtin", package, argc, symbol)
    show (RunBuiltIn package argc symbol) = show("run-builtin", package, argc, symbol)
    show (Block code) = show("block", code)
    show (RecursiveFunc args (_, body)) = show("rec-func", args, body)
    show (Infixes node operators) = show("infixes", node, operators)

type NodeWithMetaData = ((Int,Int), Node)

type Parser = Operation String ((Int,Int), Token) [NodeWithMetaData]
type ParserComponent = Operation String ((Int,Int), Token) NodeWithMetaData

parseToken :: Token -> Operation String ((Int,Int), Token) ((Int,Int), Token)
parseToken matched = filterChar errorMessage (snd >>> (matched ==))
    where 
        errorMessage ((lineNum, colNum),token) _ = 
            -- let rest = snd <$> rest' in 
                "At line " ++ show lineNum ++ ", and column " ++ show colNum ++ " : Token " ++ show token 
            ++ " does not match " ++ show matched ++ ";\n" 
            -- ++ show rest 
            -- ++ " remaining" 

-- matchToken :: (Show a1, Show a2) => Char -> Operation String ((a1, a2), Char) ((a1, a2), Char)
-- matchToken :: Token -> Operation String (a, Token) (a, Token)
-- matchToken :: Eq b1 => b1 -> Operation String ((a, b2), b1) ((a, b2), b1)
matchToken :: (Show a1, Show a2, Eq a3, Show a3) => a3 -> Operation String ((a1, a2), a3) ((a1, a2), a3)
matchToken matched = filterChar errorMessage (snd >>> (matched ==))
    where 
        errorMessage ((lineNum, columnNum), token) _ = 
            -- let rest = snd <$> rest' in 
            "At line " ++ show lineNum ++ ", and column " ++ show columnNum ++ " : Token " ++ show token ++ " does not match " 
            ++ show matched  ++ ";\n" 
            -- ++ show rest 
            -- ++ " remaining" 

parseParen :: ParserComponent
parseParen = parseToken ParenL *> parseExpr <* parseToken ParenR

parseIdentifier :: Operation String ((Int,Int), Token) ((Int,Int), String)
parseIdentifier = idOperation & eitherOutput id (fmap sequenceA $ \case
    (metaData, Identifier identifier) -> (metaData, Right identifier)
    ((lineNum, colNum), token) -> ((lineNum, colNum),
        Left $ "At line " ++ show lineNum ++ ", and column " ++ show colNum ++ " : Token " ++show token ++ " is not an identifier")
    )

parseVariable :: ParserComponent
parseVariable = second Variable <$> parseIdentifier

parseIntLiteral :: Operation String ((Int,Int), Token) ((Int,Int), Int)
parseIntLiteral = idOperation & eitherOutput id (fmap sequenceA $ \case
    (metaData, IntLiteral intLiteral) -> (metaData, Right intLiteral)
    ((lineNum, colNum), token) -> ((lineNum, colNum),
        Left $ "At line " ++ show lineNum ++ ", and column " ++ show colNum ++ " : Token " ++show token ++ " is not an int literal")
    )

parseIntNode :: ParserComponent
parseIntNode = parseIntLiteral <&> second IntNode

parseFunc :: ParserComponent
parseFunc = (\args' (metaData, expr)->let args = snd <$> args' in (metaData, Function args (metaData, expr))) 
    <$ matchToken LambdaL <*> some parseIdentifier <* matchToken LambdaR <*> parseExpr

parseVariableDef :: ParserComponent
parseVariableDef = 
    (\definitions' (metaData, body)-> let definitions = first snd <$> definitions' 
        in (metaData, VariableDef definitions (metaData, body))) 
    <$ matchToken Set <*> defs <* matchToken In <*> parseExpr
    where 
        def = (,) <$> parseIdentifier <*> parseExpr
        defs = (:) <$> def <*> many (matchToken Sep *> def)

parseIfStatement :: ParserComponent
parseIfStatement = (\cond ifTrue ifFalse-> (fst cond, IfStatement cond ifTrue ifFalse)) 
    <$ matchToken IF <*> parseExpr <* matchToken THEN <*> parseExpr <* matchToken ELSE <*> parseExpr <* matchToken END

parseClosure :: ParserComponent
parseClosure = (\expr->(fst expr, Closure expr)) 
    <$ matchToken CLOSURE <*> parseExpr

parseEvalClosure :: ParserComponent
parseEvalClosure = (\expr->(fst expr, EvalClosure expr)) 
    <$ matchToken RUNCLOSURE <*> parseExpr

parseBuiltIn :: ParserComponent
parseBuiltIn = (\(context, argc) (_, package) (_, symbol)-> (context, BuiltIn argc package symbol)) 
    <$ matchToken BUILTIN <*> parseIntLiteral <*> parseIdentifier <*> parseIdentifier

parseBlock :: ParserComponent
parseBlock = (\code->((0,0), Block code)) 
    <$ matchToken BLOCK <*> some parseCode <* matchToken END
    where 
        parseCode = parseExpr <* matchToken Sep 

parseRecursiveFunc :: ParserComponent
parseRecursiveFunc = (\args' (metaData, expr)->let args = snd <$> args' in (metaData, RecursiveFunc args (metaData, expr)))
    <$ matchToken LambdaL 
    <* matchToken (Identifier "rec")
    <*> some parseIdentifier <* matchToken LambdaR 
    <*> parseExpr

-- to avoid recursive calls
-- pretty much anything that can be put into a function call without parenthesis
parseUnit :: ParserComponent
parseUnit = asum [parseRecursiveFunc, parseBlock, parseBuiltIn, parseClosure, parseEvalClosure, parseIfStatement, parseVariableDef, parseParen, parseVariable, parseFunc, parseIntNode]

liftWithMetaData :: ((a, b1) -> b2 -> c) -> (a, b1) -> b2 -> (a, c)
liftWithMetaData f = curry (first dupFirst >>> mix >>> second (uncurry f))

parseCall :: ParserComponent
parseCall = liftWithMetaData Call <$> parseUnit <*> some parseUnit

parseNonInfix :: ParserComponent
parseNonInfix = asum [parseCall, parseUnit]

parseInfix :: ParserComponent
parseInfix = (\node infixes->(fst node, Infixes node infixes)) 
        <$> parseNonInfix <*> some parseInfixes
    where 
        parseInfixes = (,) 
            <$ matchToken INFIXL
            <*> parseExpr 
            <* matchToken INFIXR
            <*> parseNonInfix

parseExpr :: ParserComponent
parseExpr = asum [parseInfix, parseNonInfix]

parser :: Parser
parser = some parseExpr