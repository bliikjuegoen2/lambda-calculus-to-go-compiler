import Operation(runOperation, pipe)
import LineNumber (withLocation)
import Tokenizer (tokenizer)
import Parser (parser)

filename :: String
filename = "test/src.lc"

main :: IO ()
main = do 
    src <- readFile filename 

    -- let op = some $ matchChar 'a'

    let op = withLocation id `pipe` tokenizer `pipe` parser

    let x = snd $ runOperation op src

    putStrLn $ take 10000 $ show x
