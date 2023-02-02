module Tokenizer (
    Token(Identifier, ParenL, ParenR, LambdaL, LambdaR, 
        Set, In, Sep, IntLiteral, IF, THEN, ELSE, END,
        CLOSURE, RUNCLOSURE, BUILTIN, BLOCK,INFIXL, INFIXR)
    , Tokenizer
    , tokenizer
) where
import Operation (Operation, filterOutput)
import CharOperation (filterChar)
import Control.Arrow ((>>>), Arrow (first, second))
import Control.Applicative (some, Alternative (many, (<|>)), asum)
import LineNumber (Location(Location, getLocation))
import Data.Char (isSpace, isDigit)
import Data.Functor ((<&>), ($>))
import Data.List (singleton)
import Data.Function ((&))

data Token
    = Identifier String 
    | ParenL 
    | ParenR 
    | LambdaL
    | LambdaR
    | Set
    | In
    | Sep
    | IntLiteral Int
    | IF 
    | THEN 
    | ELSE
    | END
    | CLOSURE 
    | RUNCLOSURE
    | BUILTIN
    | BLOCK 
    | INFIXL
    | INFIXR
    deriving (Show, Eq)

type Tokenizer = Operation String ((Int,Int), Char) [((Int,Int), Token)]
type TokenizerComponent = Operation String ((Int,Int), Char) ((Int,Int), Token)

errorMessage :: (Show a1, Show a2, Show a3, Show (f b), Functor f) => String -> ((a1, a2), a3) -> f (a4, b) -> String
errorMessage typeOfToken ((lineNum, columnNum), char) rest' = let rest = snd <$> rest' 
            in "At line " ++ show lineNum ++ ", and column " ++ show columnNum ++ " : " ++ show char 
            ++ " is not "++ typeOfToken ++"; there is " ++ show rest ++ " remaining\n" 

isSpecial :: Char -> Bool
isSpecial char = char `elem` "()[];:$<>"

cleanMetaData :: [((Int, Int), d)] -> ((Int, Int), [d])
cleanMetaData = fmap (first Location) >>> sequenceA >>> first getLocation

getIdentifierLike :: Operation String ((Int, Int), Char) ((Int, Int), [Char])
getIdentifierLike = some (filterChar (errorMessage "an identifier") (snd >>> ((&&) <$> not . isSpecial <*> not . isSpace)))
    <&> cleanMetaData

getIdentifier :: TokenizerComponent
getIdentifier = getIdentifierLike <&> second Identifier

getIntLiteral :: TokenizerComponent
getIntLiteral = (unsignedIntLiteral <|> explicitPositiveIntLiteral <|> negativeIntLiteral) -- initial character
    <&> (cleanMetaData >>> second (read >>> IntLiteral))
    where
        unsignedIntLiteral = (:) <$> getDigit <*> many (getDigit <|> getUnderscore) 
            <&> concat 
        explicitPositiveIntLiteral = getPlusSign *> unsignedIntLiteral
        negativeIntLiteral = (:) <$> getMinusSign <*> unsignedIntLiteral
        getDigit = filterChar (errorMessage "not a digit") (snd >>> isDigit)
            <&> singleton 

        getMinusSign = filterChar (errorMessage "not a '-'") (snd >>> ('-'==))
        getPlusSign = filterChar (errorMessage "not a '+'") (snd >>> ('+'==))
            <&> singleton
        getUnderscore = filterChar (errorMessage "not a '_'") (snd >>> ('_'==))
            $> []

getCharToken :: (Show a1, Show a2) => Char -> Operation String ((a1, a2), Char) ((a1, a2), Char)
getCharToken char = filterChar (errorMessage [char]) (snd >>> (char ==))

getSpecial :: Char -> Token -> TokenizerComponent
getSpecial char token = (token <$) <$> getCharToken char

getSpecialChars :: TokenizerComponent
getSpecialChars = asum $ uncurry getSpecial <$> [
    ('(', ParenL)
    , (')', ParenR)
    , ('[', LambdaL)
    , (']', LambdaR)
    , (';', Sep)
    , (':', CLOSURE)
    , ('$', RUNCLOSURE)
    , ('<', INFIXL)
    , ('>', INFIXR)
    ]

getKeyword :: String -> Token -> TokenizerComponent
getKeyword keyword token = getIdentifierLike 
    & filterOutput (errorMessage $ "the keyword: " ++ show keyword) (snd >>> (keyword ==)) 
    <&> second (const token)

getKeywords :: TokenizerComponent
getKeywords = asum $ uncurry getKeyword <$> [
    ("set", Set)
    , ("in", In)
    , ("if", IF)
    , ("then", THEN)
    , ("else", ELSE)
    , ("end", END)
    , ("builtin", BUILTIN)
    , ("block", BLOCK)
    ]  

getSpace :: Operation String ((Int, Int), Char) ((Int, Int), [Char])
getSpace = cleanMetaData <$> many (filterChar (errorMessage "whitespace") $ snd >>> isSpace)

tokenizer :: Tokenizer
tokenizer = many (getSpace *> asum [getIntLiteral, getSpecialChars, getKeywords, getIdentifier] <* getSpace) 