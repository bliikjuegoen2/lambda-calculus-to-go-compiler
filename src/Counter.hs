module Counter (
    counterOn
    , counter
) where 
import Operation (Operation, accumOutput)
import Data.Functor ((<&>))

withCount :: a -> b -> (a, (a, b))
withCount count char = (count, (count, char))

counterOn :: (Semigroup err) => (char -> Bool) -> (char -> Bool) -> Operation err char [(Int,char)]
counterOn isReset condition 
    = accumOutput 
        (
            \accum char -> case () of
                _   | isReset char -> withCount 0 char
                    | condition char -> withCount (accum + 1) char 
                    | otherwise -> withCount accum char
        )
        (0 :: Int) 
    <&> snd

counter :: (Semigroup err, Eq char) => char -> char -> Operation err char [(Int,char)]
counter resetChar matched = counterOn (resetChar ==) (matched ==)