{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Primitive where

import Prelude (Show,Read)

---------------------------------------------
-- Синтаксис лямбда-выражений

-- Эквивалентные определения
example1 x  = x
example1'   = \x -> x
example1''  = let y = \x -> x in y
example1''' = y where
    y = \x -> x

-- Снова эквивалентные определения
example2 x y  = x %+ y
example2' x   = \y -> x %+ y
example2''    = \x -> \y -> x %+ y
example2'''   = \x y -> x %+ y
example2''''  = let z = \x y -> x %+ y in z
example2''''' = z where
    z x = \y -> x %+ y

-- Зацикленное выражение
undefined = undefined

-- Ниже следует реализовать все термы, состоящие из undefined заглушки.
-- Любые термы можно переписывать (natEq и natLt --- хорошие кандидаты).

-------------------------------------------
-- Примитивные типы

-- Тип с единственным элементом
data Unit = Unit deriving (Show,Read)

-- Пара, произведение
data Pair a b = Pair { fst :: a, snd :: b } deriving (Show,Read)

-- Вариант, копроизведение
data Either a b = Left a | Right b deriving (Show,Read)

-- Частый частный случай, изоморфно Either Unit a
data Maybe a = Nothing | Just a deriving (Show,Read)

-- Частый частный случай, изоморфно Either Unit Unit
data Bool = False | True deriving (Show,Read)

-- Следует отметить, что встроенный if с этим Bool использовать нельзя,
-- зато case всегда работает.

-- Ну или можно реализовать свой if
if' True a b = a
if' False a b = b

-- Трихотомия. Замечательный тип, показывающий результат сравнения
data Tri = LT | EQ | GT deriving (Show,Read)

-------------------------------------------
-- Булевы значения

-- Логическое "НЕ"
not :: Bool -> Bool
not True = False
not False = True

infixr 3 &&
-- Логическое "И"
(&&) :: Bool -> Bool -> Bool
True  && x = x
False && _ = False

infixr 2 ||
-- Логическое "ИЛИ"
(||) :: Bool -> Bool -> Bool
True  || _ = True
False || x = x

-------------------------------------------
-- Натуральные числа

data Nat = Zero | Succ Nat deriving (Show,Read)

natZero = Zero     -- 0
natOne = Succ Zero -- 1

-- Сравнивает два натуральных числа
natCmp :: Nat -> Nat -> Tri
natCmp Zero Zero = EQ
natCmp Zero (Succ _) = LT
natCmp (Succ _) Zero = GT
natCmp (Succ n) (Succ m) = natCmp n m

-- n совпадает с m 
natEq :: Nat -> Nat -> Bool
natEq Zero     Zero     = True
natEq Zero     (Succ _) = False
natEq (Succ _) Zero     = False
natEq (Succ n) (Succ m) = natEq n m

-- n меньше m
natLt :: Nat -> Nat -> Bool
natLt Zero     Zero     = False
natLt Zero     (Succ m) = True
natLt (Succ n) Zero     = False
natLt (Succ n) (Succ m) = natLt n m

infixl 6 +.
-- Сложение для натуральных чисел
(+.) :: Nat -> Nat -> Nat
Zero     +. m = m
(Succ n) +. m = Succ (n +. m)

infixl 6 -.
-- Вычитание для натуральных чисел
(-.) :: Nat -> Nat -> Nat
Zero -. _ = Zero
m -. Zero = m
(Succ n) -. (Succ m) = n -. m

infixl 7 *.
-- Умножение для натуральных чисел
(*.) :: Nat -> Nat -> Nat
Zero     *. m = Zero
(Succ n) *. m = m +. (n *. m)

-- Целое и остаток от деления n на m
natDivMod :: Nat -> Nat -> Pair Nat Nat
natDivMod n m = if' (natLt n m) (Pair Zero n) (let x = (natDivMod (n -. m) m) in Pair (Succ (fst x)) (snd x))

natDiv n = fst . natDivMod n -- Целое
natMod n = snd . natDivMod n -- Остаток

-- Поиск GCD алгоритмом Евклида (должен занимать 2 (вычислителельная часть) + 1 (тип) строчки)
gcd :: Nat -> Nat -> Nat
gcd n Zero = n
gcd n m = if' (natLt n m) (gcd m n) (gcd m (natMod n m))

-------------------------------------------
-- Целые числа

-- Требуется, чтобы представление каждого числа было единственным
-- lets assume Int Zero Neg == -1, Int natOne Neg = -2 and so on
data Sign = Pos | Neg deriving (Show, Read)
data Int = Int Nat Sign deriving (Show,Read)

intZero   = Int Zero Pos   -- 0
intOne    = Int (Succ Zero) Pos     -- 1
intNegOne = Int Zero Neg

-- n -> - n
intNeg :: Int -> Int
intNeg (Int Zero Pos) = Int Zero Pos
intNeg (Int m Pos) = Int (m -. natOne) Neg
intNeg (Int m Neg) = Int (Succ m) Pos

-- Дальше также как для натуральных
intCmp :: Int -> Int -> Tri
intCmp (Int _ Pos) (Int _ Neg) = GT
intCmp (Int _ Neg) (Int _ Pos) = LT
intCmp (Int n Pos) (Int m Pos) = natCmp n m
intCmp (Int n Neg) (Int m Neg) = natCmp m n

intEq :: Int -> Int -> Bool
intEq (Int _ Pos) (Int _ Neg) = False
intEq (Int _ Neg) (Int _ Pos) = False
intEq (Int n Pos) (Int m Pos) = natEq n m
intEq (Int n Neg) (Int m Neg) = natEq n m

intLt :: Int -> Int -> Bool
intLt (Int _ Pos) (Int _ Neg) = False
intLt (Int _ Neg) (Int _ Pos) = True
intLt (Int n Pos) (Int m Pos) = natLt n m
intLt (Int n Neg) (Int m Neg) = natLt m n

infixl 6 .+., .-.
-- У меня это единственный страшный терм во всём файле
(.+.) :: Int -> Int -> Int
(Int n Pos) .+. (Int m Pos) = Int (m +. n) Pos
(Int n Neg) .+. (Int m Neg) = Int (m +. n +. natOne) Neg
(Int n Pos) .+. (Int m Neg) = if' (natLt m n) (Int (n -. m -. natOne) Pos) (Int (m -. n) Neg)
(Int n Neg) .+. (Int m Pos) = (Int m Pos) .+. (Int n Neg)

(.-.) :: Int -> Int -> Int
n .-. m = n .+. (intNeg m)

infixl 7 .*.
(.*.) :: Int -> Int -> Int
(Int n Pos) .*. (Int m Pos) = Int (m *. n) Pos
(Int n Neg) .*. (Int m Neg) = Int ( (m +. natOne) *. (n +. natOne) ) Pos
(Int Zero Pos) .*. (Int _ Neg) = Int Zero Pos
(Int n Pos) .*. (Int m Neg) = Int ( (m) *. (n +. natOne) ) Neg
(Int n Neg) .*. (Int m Pos) = (Int m Pos) .*. (Int n Neg)

-------------------------------------------
-- Рациональные числа

data Rat = Rat Int Nat deriving (Show, Read)

ratNeg :: Rat -> Rat
ratNeg (Rat x y) = Rat (intNeg x) y

-- У рациональных ещё есть обратные элементы
ratInv :: Rat -> Rat
ratInv (Rat (Int x Pos) y) = Rat (Int y Pos) x
ratInv (Rat (Int x Neg) y) = Rat (Int (y -. natOne) Neg) (Succ x)

-- Дальше как обычно
ratCmp :: Rat -> Rat -> Tri
ratCmp (Rat x y) (Rat z w) = intCmp (x .*. (Int w Pos)) (z .*. (Int y Pos))

ratEq :: Rat -> Rat -> Bool
ratEq (Rat x y) (Rat z w) = intEq (x .*. (Int w Pos)) (z .*. (Int y Pos))

ratLt :: Rat -> Rat -> Bool
ratLt (Rat x y) (Rat z w) = intLt (x .*. (Int w Pos)) (z .*. (Int y Pos))

infixl 7 %+, %-
(%+) :: Rat -> Rat -> Rat
(Rat x y) %+ (Rat z w) = Rat ( ( x .*. (Int w Pos) ) .+. (z .*. (Int y Pos) ) ) (y *. w)

(%-) :: Rat -> Rat -> Rat
n %- m = n %+ (ratNeg m)

infixl 7 %*, %/
(%*) :: Rat -> Rat -> Rat
(Rat x y) %* (Rat z w) = Rat ( x .*. z ) ( y *. w ) 

(%/) :: Rat -> Rat -> Rat
n %/ m = n %* (ratInv m)

-------------------------------------------
-- Операции над функциями.
-- Определены здесь, но использовать можно и выше

infixr 9 .
f . g = \ x -> f (g x)

infixr 0 $
f $ x = f x

-- Эквивалентные определения
example3   a b c = gcd a (gcd b c)
example3'  a b c = gcd a $ gcd b c
example3'' a b c = ($) (gcd a) (gcd b c)

-- И ещё эквивалентные определения
example4  a b x = (gcd a (gcd b x))
example4' a b = gcd a . gcd b


--- Штуки чтобы тестить листы
odd :: Nat -> Bool
odd Zero = False
odd (Succ x) = even x

even :: Nat -> Bool
even Zero = True
even (Succ x) = odd x

odd_maybe :: Nat -> Maybe Nat
odd_maybe x = case (odd x) of
                True -> Just x
                False -> Nothing

even_maybe :: Nat -> Maybe Nat
even_maybe x = case (even x) of
                True -> Just x
                False -> Nothing
