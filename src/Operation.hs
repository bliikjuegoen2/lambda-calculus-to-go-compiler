{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Operation (
    OperationOutput
    , ret 
    , raise
    , Operation
    , runOperation
    , BasicError(endOfStream,notEndOfStream)
    , idOperation
    , modifyOutputWithRest
    , modifyOutput
    , filterOutput
    , matchOutput
    , accumOutput
    , pipe
) where
import Control.Applicative (Alternative (empty, (<|>)))
import Control.Arrow ((>>>))
import Data.List (mapAccumL)

data OperationOutput err output
    = Output output
    | Err err
    deriving (Show)

ret :: output -> OperationOutput err output
ret = Output

raise :: err -> OperationOutput err output
raise = Err

-- Operation Output is just a Semigroup, NOT a monoid
-- The semigroup prioritizes errors over result

instance (Semigroup err) => Semigroup (OperationOutput err output) where
    Err erra <> Err errb = Err $ erra <> errb
    Err err <> _ = Err err 
    _ <> Err err = Err err 
    Output x <> _ = Output x

instance Foldable (OperationOutput err) where 
    foldMap f (Output x) = f x 
    foldMap _ (Err _) = mempty 

instance Traversable (OperationOutput err) where 
    traverse f (Output x) = Output <$> f x
    traverse _ (Err err) = pure $ Err err

instance Functor (OperationOutput err) where 
    fmap f (Output x) = Output $ f x 
    fmap _ (Err err) = Err err

instance (Semigroup err) => Applicative (OperationOutput err) where 
    pure = Output

    Output f <*> Output x = Output $ f x 
    Err err1 <*> Err err2 = Err $ err1 <> err2
    Err err <*> _ = Err err 
    _ <*> Err err = Err err

-- The alternative of operation output prefers success operations over failed operations

instance (Monoid err) => Alternative (OperationOutput err) where 
    empty = Err mempty

    Output x <|> _ = Output x
    _ <|> Output x = Output x
    Err err1 <|> Err err2 = Err $ err1 <> err2 

instance (Semigroup err) => Monad (OperationOutput err) where 
    Output x >>= f = f x 
    Err err >>= _ = Err err

newtype Operation err input output = Operation {runOperation :: [input] -> ([input], OperationOutput err output)}

instance (Semigroup err) => Functor (Operation err input) where 
    fmap f operationX = Operation $ \input-> sequenceA $ do 
        (rest,x) <- sequenceA $ runOperation operationX input
        return (rest, f x)

instance (Semigroup err) => Applicative (Operation err input) where
    pure x = Operation (,pure x)

    operationF <*> operationX = Operation $ \input-> sequenceA $ do 
        (rest,f) <- sequenceA $ runOperation operationF input
        (rest',x) <- sequenceA $ runOperation operationX rest
        return (rest', f x)

instance (Monoid err) => Alternative (Operation err input) where
    empty = Operation (, empty)

    a <|> b = Operation $ \input-> runOperation a input `pick` runOperation b input
        where 
            pick (rest, Output x) (_, _) = (rest, Output x)
            pick (_, _) (rest, Output x) = (rest, Output x)
            pick (rest, Err err1) (_, Err err2) = (rest, Err $ err1 <> err2)

instance (Semigroup err) => Monad (Operation err input) where
    operationX >>= f = Operation $ \input-> sequenceA $ do 
        (rest,x) <- sequenceA $ runOperation operationX input
        sequenceA $ runOperation (f x) rest

class BasicError err where 
    endOfStream :: err 
    notEndOfStream :: (Show input) => [input] -> err

idOperation :: (BasicError err) => Operation err a a 
idOperation = Operation $ \case 
    x:xs -> (xs, ret x)
    [] -> ([], raise endOfStream)

modifyOutputWithRest :: (([input], OperationOutput err output) -> ([input], OperationOutput err' output')) -> Operation err input output -> Operation err' input output'
modifyOutputWithRest f x = Operation $ runOperation x >>> f

modifyOutput :: (OperationOutput err output -> OperationOutput err' output') -> Operation err input output -> Operation err' input output'
modifyOutput = modifyOutputWithRest . fmap

-- filterOutput :: (a -> [a] -> err) -> (a -> Bool) -> Operation err a a -> Operation err a a 
filterOutput :: (t -> [input] -> err') -> (t -> Bool) -> Operation err' input t -> Operation err' input t
filterOutput makeError condition = modifyOutputWithRest $ \(rest, output)-> case output of 
    Output x 
        | condition x -> (rest, Output x)
        | otherwise -> (rest, raise $ makeError x rest)
    Err err -> (rest, Err err)

matchOutput :: (Eq output) => (output -> output -> [input] -> err) -> output -> Operation err input output -> Operation err input output
matchOutput makeError matched = filterOutput (makeError matched) (matched ==)

accumOutput :: (s -> a -> (s, b)) -> s -> Operation err a (s, [b])
accumOutput f accum = Operation $ \input-> ([], Output $ mapAccumL f accum input)

pipe :: (Semigroup err, BasicError err, Show b) => Operation err a [b] -> Operation err b c -> Operation err a c
pipe f g = Operation $ \input-> sequenceA $ do 
    (rest, outputF) <- sequenceA $ runOperation f input
    (rest', outputG) <- sequenceA $ runOperation g outputF
    if not $ null rest' 
    then raise $ notEndOfStream rest'
    else return (rest,outputG)