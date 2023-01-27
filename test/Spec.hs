import Operation(runOperation, pipe)
import LineNumber (withLocation)
import Tokenizer (tokenizer)

filename :: String
filename = "test/src.lc"

main :: IO ()
main = do 
    src <- readFile filename 

    -- let op = some $ matchChar 'a'

    let op 
            = withLocation id `pipe` tokenizer

    let x = snd $ runOperation op src

    print x
