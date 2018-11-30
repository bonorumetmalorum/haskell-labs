data Name = Name [Char] deriving (Show, Eq)

data Lam = Var Name | Abs Name Lam| App Lam Lam deriving (Show, Eq)

data Set = Set Name Set | Nil deriving (Show, Eq)

add :: Name -> Set -> Set
add name Nil = Set name Nil
add name (Set x xs) = add name xs

join :: Set -> Set -> Set
join Nil y = y
join (Set x xs) y = Set x (join xs y)

remove :: Name -> Set -> Set
remove name Nil = Nil
remove name (Set x xs) = if name == x then xs else Set x (remove name xs)

free_variables :: Lam -> Set
free_variables (Var name) = Set name Nil
free_variables (Abs name lam) = remove name (free_variables lam)
free_variables (App lamx lamy) = join (free_variables lamx) (free_variables lamy)

is_closed :: Lam -> Bool
is_closed lam = if (free_variables lam) == Nil then True else False

subst :: Name -> Lam -> Lam -> Lam
subst name v lam = go (remove name (free_variables lam)) where go set | Nil = lam |(Set name others) = subst name v lam
subst name v (App lamx lamy) = App (subst name v lamx) (subst name v lamy)
subst name v (Abs param lam) = if name == param then lam else subst name V lam