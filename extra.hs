import Data.Maybe
--LISTS
lastly::[a] -> Maybe a
lastly [] = Nothing
lastly (h:[]) = Just h
lastly (h:t) =  lastly t

penult::[a] -> Maybe a
penult [] = Nothing
penult (h:t:[]) = Just h
penult (h:t) = penult t

largest::(Ord a) => [a] -> a -> a
largest [] acc = acc
largest (h:t) acc = if h > acc then largest t h else largest t acc

smallest::(Ord a) => [a] -> a -> a
smallest [] acc = acc
smallest (h:t) acc = if h < acc then smallest t h else smallest t acc
