import CharOperation(filterChar)
import Operation(runOperation, pipe, idOperation)
import Control.Applicative (some)
import Counter (counterOn)
import Data.Functor ((<&>))
import LineNumber (withLocation)

filename :: String
filename = "test/src.lc"

main :: IO ()
main = do 
    src <- readFile filename 

    -- let op = some $ matchChar 'a'

    let op 
            = withLocation id
            `pipe` ((,) <$> some (filterChar (\((linenumber, col), char) rest' -> 
                let rest = snd <$> rest' in "On line " ++ show linenumber ++ " column " ++ show col ++ ": Cannot match 'a' with " ++ show char ++ "; there is " ++ show rest ++ " left") 
            ((== 'a') . snd)) <*> some idOperation)

    let x = snd $ runOperation op src

    print x
