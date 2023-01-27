import CharOperation(filterChar)
import Operation(runOperation, pipe, idOperation)
import Control.Applicative (some)
import Counter (counterOn)
import Data.Functor ((<&>))

filename :: String
filename = "test/src.lc"

mix ((a,b),c) = (a,(b,c))

remix (a,(b,c)) = ((a,b),c)

main :: IO ()
main = do 
    src <- readFile filename 

    -- let op = some $ matchChar 'a'

    let op 
            = (
                counterOn (== '\n') (const True) 
                `pipe` counterOn (const False) ((== '\n') . snd) 
                <&> fmap remix
            ) 
            `pipe` ((,) <$> some (filterChar (\((linenumber, col), char) rest' -> 
                let rest = snd <$> rest' in "On line " ++ show linenumber ++ " column " ++ show col ++ ": Cannot match 'a' with " ++ show char ++ "; there is " ++ show rest ++ " left") 
            ((== 'a') . snd)) <*> some idOperation)

    let x = snd $ runOperation op src

    print x
