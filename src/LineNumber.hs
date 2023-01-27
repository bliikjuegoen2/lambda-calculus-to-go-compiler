module LineNumber (
    lineNumber
    , columnNumber 
    , withLocation
    , Location(Location)
    , getLocation
) where
import Operation (Operation, BasicError, compose)
import Counter (counterOn)
import Control.Arrow ( (>>>) )
import Util (remix)

lineNumber :: (Semigroup err) => (a -> Char) -> Operation err a [(Int, a)]
lineNumber toChar = counterOn (const False) $ toChar >>> ('\n' ==)

columnNumber :: (Semigroup err) => (a -> Char) -> Operation err a [(Int, a)]
columnNumber toChar = counterOn (toChar >>> ('\n' ==)) $ const True

withLocation :: (Semigroup err, BasicError err, Show a) => (a -> Char) -> Operation err a [((Int, Int), a)]
withLocation toChar = fmap remix <$> lineNumber (snd >>> toChar) `compose` columnNumber toChar

newtype Location = Location { getLocation :: (Int,Int) }

instance Semigroup Location where 
    Location (-1,-1) <> x = x
    x <> Location (-1,-1) = x 
    x <> _ = x 

instance Monoid Location where 
    mempty = Location (-1,-1)
