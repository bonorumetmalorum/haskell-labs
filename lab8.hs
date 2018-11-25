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

data Bexp = And Bexp Bexp | Equal Aexp Aexp | Lt Aexp Aexp | Not Bexp

evalB::Bexp -> Memory -> Bool
evalB (And bexp1 bexp2) mem = (evalB bexp1 mem) && (evalB bexp2 mem)
evalB (Not bexp) mem = inverse (evalB bexp mem) where inverse = \f -> if f then False else True
evalB (Equal exp1 exp2) mem = (evalA exp1 mem) == (evalA exp2 mem)
evalB (Lt exp1 exp2) mem = (evalA exp1 mem) < (evalA exp2 mem)

data Comm = Ass Name Integer | Seq Comm Comm | If Bexp Comm Comm | While Bexp Comm

evalC::Comm -> Memory -> Memory
evalC (Seq x y) mem = evalC y (evalC x mem) 
evalC (Ass n v) mem = update n v mem
evalC (If bexp comx comy) mem = if evalB bexp mem then evalC comx mem else evalC comy mem
evalC (While bexp com) mem = if evalB bexp mem then evalC (While bexp com) (evalC com mem) else mem

