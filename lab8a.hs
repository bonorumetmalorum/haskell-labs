module Language.Imperative(
    Name,
    Aexp,
    Bexp,
    Comm,
    Memory,
    evalA,
    evalB,
    evalC,
    find,
    update
)where


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

data Comm = Ass Name Aexp | Ref Name Name | Seq Comm Comm | If Bexp Comm Comm | While Bexp Comm

evalC::Comm -> Memory -> Memory
evalC (Seq x y) mem = evalC y (evalC x mem) 
evalC (Ass n exp) mem = update n (evalA exp mem) mem
evalC (If bexp comx comy) mem = if evalB bexp mem then evalC comx mem else evalC comy mem
evalC (While bexp com) mem = if evalB bexp mem then evalC (While bexp com) (evalC com mem) else mem

example1 = (Seq (Ass (Name "x") (Number 1)) (Seq (Ass (Name "y") (Var (Name "x"))) (Ass (Name "x") (Var (Name "z")))))
example2 = (Seq (Ass (Name "z") (Number 5)) (Seq (Ass (Name "x") (Number 4)) (If (Lt (Var (Name "z")) (Var (Name "x"))) (Ass (Name "y") (Var (Name "z"))) (Ass (Name "y") (Var (Name "x"))))))
mem = (Mem (Name "x", 0) (Mem (Name "y", 3) Nil))
example3 = (While (Lt (Var (Name "x")) (Var (Name "y"))) (Ass (Name "x") (Add (Number 1) (Var (Name "x")))))
