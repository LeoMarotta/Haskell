data Dia = Segunda | Terça | Quarta | Quinta | Sexta | Sabado | Domingo
    deriving(Eq,Show)

finalDeSemana :: Dia -> Bool
finalDeSemana Sabado = True
finalDeSemana Domingo = True
finalDeSemana _ = False

data TalvezFloat = Valor Float | Erro String
    deriving(Eq,Show)

divisao :: Float -> Float -> TalvezFloat
divisao n1 n2
    | (n2 /= 0) = Valor (n1/n2)
    | otherwise = Erro "nao e possivel dividir por 0"

data Nat = Zero | Suc Nat
    deriving(Eq,Show)

natToint :: Nat -> Int
natToint Zero = 0
natToint (Suc x) = 1 + natToint x

intToNat :: Int -> Nat
intToNat 0 = Zero
intToNat x = (Suc (intToNat (x-1)))