
type FList a = Maybe Int -> [a]

empty :: FList a
empty = \ _ -> []

add :: a -> FList a -> FList a
add x fl = \ pre -> 
  case pre of
    Nothing        -> x : fl Nothing
    Just n | n > 0 -> x : (fl $ Just $ n-1)
    _              -> []
        
fromList :: [a] -> FList a
fromList xs = foldr add empty xs

toList :: FList a -> [a]
toList fl = fl Nothing
        
ftake :: Int -> FList a -> [a]
ftake n fl = fl $ Just n