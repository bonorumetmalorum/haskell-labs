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

s_n_l::(Ord a, Num a) => [a] -> (a, a)
s_n_l list = foldr (\a acc -> (if a < (fst acc) then a else fst acc, if a > (snd acc) then a else snd acc)) (999999, 0) list

no_dup::(Eq a) => [a] -> [a]
no_dup list = foldr (\a acc -> case acc of [] -> [a]; (h:t) -> if a == h then acc else [a] ++ acc) [] list

dup::(Eq a) => [a] -> [a]
dup list = foldr (\a acc -> a:a:acc) [] list