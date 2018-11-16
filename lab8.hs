data Name = Name String deriving (Show, Eq)

data Memory =  Mem (Name, Integer) Memory | Nil deriving Show

update::Name -> Integer -> Memory -> Memory
update name val Nil = Mem (name, val) Nil
update name val (Mem entry next)
    |name == (fst entry) = Mem (name, val) next
    |otherwise = Mem entry (update name val next)


find :: Name -> Memory -> Integer
find name Nil = 0
find name (Mem entry next) = if name == (fst entry) then snd entry else find name next


data Aexp = Number Integer | Var Name | Add Aexp Aexp | Mult Aexp Aexp

evalA::Aexp -> Memory -> Integer
evalA (Var name) memory = find name memory
evalA (Number x) _ = x
evalA (Add x y) memory = (evalA x memory) + (evalA y memory)
evalA (Mult x y) memory = (evalA x memory) * (evalA y memory)