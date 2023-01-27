{-# LANGUAGE LambdaCase #-}
module Parser (
    Node(Variable, Function, Call, VariableDef)
    , parser
    , parseExpr
) where
    
import Operation (Operation, eitherOutput, idOperation)
import Tokenizer (Token (ParenL, ParenR, Identifier, LambdaL, LambdaR, Set, In, Sep))
import Control.Applicative (many, asum, Alternative (some))
import CharOperation (filterChar)
import Data.Function ((&))
import Control.Arrow ((>>>), Arrow (first, second))
import Util (dupFirst, mix)

data Node
    = Variable String
    | Function [String] NodeWithMetaData
    | Call NodeWithMetaData [NodeWithMetaData]
    | VariableDef [(String, NodeWithMetaData)] NodeWithMetaData

instance Show Node where 
    show (Variable var) = show ("var", var)
    show (Function args (_, body)) = show ("func", args, body)
    show (Call (_, func) args') = let args = fmap snd args' in show ("call", func, args)
    show (VariableDef vars' (_, body)) = let var = second snd <$> vars' in show ("def", var, body)

type NodeWithMetaData = ((Int,Int), Node)

type Parser = Operation String ((Int,Int), Token) [NodeWithMetaData]
type ParserComponent = Operation String ((Int,Int), Token) NodeWithMetaData

parseToken :: Token -> Operation String ((Int,Int), Token) ((Int,Int), Token)
parseToken matched = filterChar errorMessage (snd >>> (matched ==))
    where 
        errorMessage ((lineNum, colNum),token) rest' = let rest = snd <$> rest'
            in "At line " ++ show lineNum ++ ", and column " ++ show colNum ++ " : Token " ++ show token 
            ++ " does not match " ++ show matched ++ "; there is " ++ show rest ++ " remaining" 

-- matchToken :: (Show a1, Show a2) => Char -> Operation String ((a1, a2), Char) ((a1, a2), Char)
-- matchToken :: Token -> Operation String (a, Token) (a, Token)
-- matchToken :: Eq b1 => b1 -> Operation String ((a, b2), b1) ((a, b2), b1)
matchToken :: (Show a1, Show a2, Eq a3, Show a3) => a3 -> Operation String ((a1, a2), a3) ((a1, a2), a3)
matchToken matched = filterChar errorMessage (snd >>> (matched ==))
    where 
        errorMessage ((lineNum, columnNum), token) rest' = let rest = snd <$> rest' 
            in "At line " ++ show lineNum ++ ", and column " ++ show columnNum ++ " : Token " ++ show token ++ " does not match " 
            ++ show matched  ++ "; there is " ++ show rest ++ " remaining" 

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


-- to avoid recursive calls
-- pretty much anything that can be put into a function call without parenthesis
parseUnit :: ParserComponent
parseUnit = asum [parseParen, parseVariable, parseFunc, parseVariableDef]

liftWithMetaData :: ((a, b1) -> b2 -> c) -> (a, b1) -> b2 -> (a, c)
liftWithMetaData f = curry (first dupFirst >>> mix >>> second (uncurry f))

parseCall :: ParserComponent
parseCall = liftWithMetaData Call <$> parseUnit <*> some parseUnit


parseExpr :: ParserComponent
parseExpr = asum [parseCall, parseUnit]

parser :: Parser
parser = many parseExpr