data Name = Name String deriving (Show, Eq)

data Memory =  Mem (Name, Integer) Memory | Nil deriving Show

update::Name -> Integer -> Memory -> Memory
update name val Nil = Mem (name, val) Nil
update name val (Mem entry next)
    |name == (fst entry) = Mem (name, val) next
    |otherwise = Mem entry (update name val next)