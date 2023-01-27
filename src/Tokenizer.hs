module Tokenizer (
    Token(Identifier, ParenL, ParenR, LambdaL, LambdaR, Set, In, Sep)
    , Tokenizer
    , tokenizer
) where
import Operation (Operation)
import CharOperation (filterChar)
import Control.Arrow ((>>>), Arrow (first, second))
import Control.Applicative (some, Alternative (many), asum)
import LineNumber (Location(Location, getLocation))
import Data.Char (isSpace)

data Token
    = Identifier String 
    | ParenL 
    | ParenR 
    | LambdaL
    | LambdaR
    | Set
    | In
    | Sep
    deriving (Show, Eq)

type Tokenizer = Operation String ((Int,Int), Char) [((Int,Int), Token)]
type TokenizerComponent = Operation String ((Int,Int), Char) ((Int,Int), Token)

errorMessage :: (Show a1, Show a2, Show a3, Show (f b), Functor f) => String -> ((a1, a2), a3) -> f (a4, b) -> String
errorMessage typeOfToken ((lineNum, columnNum), char) rest' = let rest = snd <$> rest' 
            in "At line " ++ show lineNum ++ ", and column " ++ show columnNum ++ " : " ++ show char 
            ++ " is not "++ typeOfToken ++"; there is " ++ show rest ++ " remaining" 

isSpecial :: Char -> Bool
isSpecial char = char `elem` "()[]"

cleanMetaData :: [((Int, Int), d)] -> ((Int, Int), [d])
cleanMetaData = fmap (first Location) >>> sequenceA >>> first getLocation

getIdentifier :: TokenizerComponent
getIdentifier = (cleanMetaData >>> second Identifier) 
        <$> some (filterChar (errorMessage "an identifier") (snd >>> ((&&) <$> not . isSpecial <*> not . isSpace))) 

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
    ]

getKeyword :: String -> Token -> TokenizerComponent
getKeyword keyword token = (sequenceA >>> fmap (cleanMetaData >>> fmap (const token))) $ getCharToken <$> keyword

getKeywords :: TokenizerComponent
getKeywords = asum $ uncurry getKeyword <$> [
    ("set", Set)
    , ("in", In)
    ]  

getSpace :: Operation String ((Int, Int), Char) ((Int, Int), [Char])
getSpace = cleanMetaData <$> many (filterChar (errorMessage "whitespace") $ snd >>> isSpace)

tokenizer :: Tokenizer
tokenizer = many (getSpace *> asum [getSpecialChars, getKeywords, getIdentifier] <* getSpace) 