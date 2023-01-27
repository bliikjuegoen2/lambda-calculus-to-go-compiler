{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
module CharOperation (
    filterChar
    , matchChar
) where
import Operation (Operation, filterOutput, idOperation, BasicError (endOfStream, notEndOfStream))
import Data.Function ((&))

instance BasicError String where 
    endOfStream = "End of the Stream"
    notEndOfStream rest out= "Not the end of Stream; " ++ show out++ " was parsed; there is " ++ show rest ++ " left"

filterChar :: (char -> [char] -> String) -> (char -> Bool) -> Operation String char char 
filterChar makeError condition = idOperation & filterOutput makeError condition

matchChar :: (Show a, Eq a) => a -> Operation String a a
matchChar matched = filterChar (\char rest->"Cannot match " ++ show matched ++ " with " ++ show char ++ "; there is " ++ show rest ++ " left") (matched ==)