{- 
1. Работа с типом Expr. Используя тип Expr, определенный выше, реализуйте следующие функции 
(используйте для тестирования функцию parseExpr)
1) Определите корректную функцию diff, которая принимает в качестве дополнительного 
аргумента имя переменной, по ко торой необходимо осуществлять дифференцирование.
-}

data Expr = Const Integer | Add Expr Expr | Mult Expr Expr | Var String deriving (Eq,Show)
diff_ :: Expr -> String -> Expr
diff_ (Const _) _ = Const 0
diff_ (Var x) v = if (x == v) then Const 1 else Const 0 
diff_ (Add x y) v = Add (diff_ x v) (diff_ y v)
diff_ (Mult x y) v = Add (Mult (diff_ x v) y) (Mult x (diff_ y v))
{-
-2) Определите функцию simplify, которая упрощает выражения типа Expr, 
применяя очевидные правила вида: 
• x + 0 = 0 + x = x
• x · 1 = 1 · x = x
• x · 0 = 0 · x = 0
-}
simplify :: Expr -> Expr
simplify (Const c) = (Const c)
simplify (Var v) = (Var v)
simplify (Add (Const n) (Const m)) = (Const (n+m))
simplify (Mult (Const n) (Const m))= (Const (n*m))
simplify (Add (Const 0) x)  = (simplify x)
simplify (Add x (Const 0))  = (simplify x)
simplify (Mult (Const 1) x) = (simplify x)
simplify (Mult x (Const 1)) = (simplify x)
simplify (Mult (Const 0) x) = (Const 0)  
simplify (Mult x (Const 0)) = (Const 0)  
simplify (Add x y)          = (Add (simplify x) (simplify y))
simplify (Mult x y)         = (Mult (simplify x) (simplify y))

{-
3) Определите функцию toString, преобразующую выражение типа Expr в строку. 
Например, результатом применения функции к выражению Add (Mult (Const 2) (Var "x")) (Var "y")
должна быть строка "2*x+y". Учтите возможность использования скобок, например, 
выражение Mult (Const 2) (Add (Var "x") (Var "y")) должно преобразовываться в строку "2*(x+y)"
-}
toString :: Expr -> String
toString (Const n) = show n
toString (Var x) = x
toString (Add x y) = "(" ++ (toString x) ++ "+" ++ (toString y) ++ ")"
toString (Mult x y) = "(" ++ (toString x) ++ "*" ++ (toString y) ++ ")"

{-
4) Определите функцию eval, которая принимает два параметра: 
выражение типа Expr и список пар типа (String,Integer), 
задающий соответствие имен переменных и их значений. Функция 
должна вычислять значение выражение с учетом заданных значений выражений. 
Например, выражение eval (Add (Var "x") (Var "y")) [("x",1),("y",2)]
 должно выдавать число 3.
-}
data Expr = Const Integer | Add Expr Expr | Mult Expr Expr | Var String deriving (Eq,Show)
getVal :: [(String,Integer)] -> String -> Maybe Integer
getVal [] _ = Nothing
getVal (vl:vls) v | (v == fst vl) = Just $ snd vl
                  | otherwise = getVal vls v
 
fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing  = error "bad variable name"
                  
eval :: Expr -> [(String,Integer)] -> Integer
eval (Const x) vlist = x
eval (Add e1 e2) vlist = (eval e1 vlist) + (eval e2 vlist)
eval (Mult e1 e2) vlist = (eval e1 vlist) * (eval e2 vlist)
eval (Var v) vlist = fromJust (getVal vlist v)

{-
 2. Функции для работы с типом List.
 Для введенного ранее типа List определите следующие функции:
  1) lengthList, возвращающую длину списка типа List.
  2) nthList, возвращающую n-й элемент списка.
  3) removeNegative, которая из списка целых (тип List Integer) удаляет отрицательные элементы.
  4) fromList, преобразующую список типа List в обычный список.
  5) toList, преобразующую обычный список в список типа List.
-}

data List a = NIL | Cons a (List a) deriving (Eq,Show)

lengthList :: List Int -> Int
lengthList (Cons x xs) = 1+lengthList xs
lengthList NIL = 0

nthList :: List Int -> Int -> Int
nthList NIL n = 0
nthList (Cons x xs) n = if n/=1 then nthList xs (n-1) else x

removeNegative :: List Int -> List Int
removeNegative NIL = NIL
removeNegative (Cons x xs) = if x>0 then Cons x (removeNegative xs) else removeNegative xs

fromList :: List Int -> [Int]
fromList NIL = []
fromList (Cons x xs) = x:fromList xs

toList :: [Int] -> List Int
toList [] = NIL
toList (x:xs) = Cons x (toList xs)

{-
Функции работы с бинарными деревьями поиска. Определите тип данных, 
представляющий бинарные деревья поиска. В отличие от
деревьев, представленных в методических указаниях, 
в деревьях поиска данные могут находиться не только в листьях, 
но и в промежуточных узлах дерева. Будем использовать деревья 
для представления ассоциативного массива, сопоставляющие значения
ключей (представляемых как строки) целым числам. Для каждого узла
с некоторым ключом в левом поддереве должны содержаться элементы с
меньшими значениями ключа, а в правом — с большими. При поиске соответствия 
между строкой и числом необходимо учитывать эту информацию, поскольку она 
позволяет более эффективно извлекать информацию из дерева.

Определите описанный тип данных и следующие функции:

1) add, добавляющую в дерево заданную пару ключа и значения.
2) find, возвращающую число, соответствующее заданной строке.
3) exists, проверяющую, что элемент с заданным ключом со-держится в дереве.
4) toList, преобразующая заданное дерево поиска в список, упорядоченный по значениям ключей.
-}

--import Data.Maybe
 
data STree = Tip | Bin (String, Int) STree STree deriving Show

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _       = True

add (k, v) Tip = Bin (k, v) Tip Tip
add (k, v)(Bin (k1, v1) l r) = 
  if k > k1 then Bin (k1, v1) l (add (k, v) r) 
  else Bin (k1, v1) (add (k, v) l) r
 
find k Tip = Nothing 
find k (Bin (k1, v1) l r) 
  | k == k1 = Just v1
  | otherwise = find k (if k > k1 then r else l)
 
exist k = isJust . find k
 
toList Tip = []
toList( Bin (_, v1) l r) = concat [toList l, [v1], toList r]
{-
Разработать тип данных, представляющий содержимое каталога файловой системы. 
Считаем, что каждый файл либо содержит некоторые данные, либо является каталогом. 
Каталог включает в себя другие файлы (которые, в свою очередь могут быть каталогами) 
вместе с их именами и размерами в байтах. В данной работе содержимое файлов можно игнорировать: 
тип данных должен представлять только их имена, размеры и структуру каталогов. 
Определите следующие функции:
  1) dirAll, возвращающую список полных имен всех файлов каталога, включая подкаталоги.
  2) find, возвращающая путь, ведущий к файлу с заданным именем. Например, если каталог 
     содерит файлы a, b и c, и b является каталогом, содержащим x и y, тогда функция поиска
     для x должна вернуть строку "b/x"
  3) du, для заданного каталога возвращает количество байт, занимаемых его файлами (включая файлы в подкаталогах).
-}
type MyFile = (String,Integer)
data MySystem =   FFile MyFile
                | FDir  (String,[MySystem]) deriving Show
du :: MySystem -> Integer
 
du (FDir (x,[])) = 0
du (FFile x)     = snd(x)
du (FDir  (x,y)) = du(head(y))+du(FDir (x,tail(y)))  
dirscan :: (String,MySystem) -> String
dirscan (c,(FDir (x,[]))) = []
dirscan (c,(FFile x))     = c++"/"++fst(x)
dirscan (c,(FDir(x,y)))   = if (c=="/") then
                              dirscan( c++x ,head(y) )++"\n"++dirscan( c, FDir(x,tail(y)) ) 
                           else
                              dirscan( c++"/"++x ,head(y) )++"\n"++dirscan( c, FDir(x,tail(y)) ) 
dirAll :: MySystem -> String
dirAll (FFile x)     = fst(x)
dirAll (FDir (x,[])) = []
dirAll (FDir (x,y))  = dirscan("/",FDir(x,y))
fscan :: (String,String,MySystem) -> String
fscan (n,c,(FDir (x,[]))) = []
fscan (n,c,(FFile x))     = if fst(x) == n then 
                               c++"/"++fst(x)++" "++show(snd(x))++"\n"
                            else
                               ""
fscan (n,c,(FDir(x,y)))   = if (c=="/") then
                              fscan( n, c++x ,head(y) )++fscan( n, c, FDir(x,tail(y)) ) 
                           else
                              fscan( n, c++"/"++x ,head(y) )++fscan( n, c, FDir(x,tail(y)) )
find :: (String,MySystem) -> String
find (n,x) = fscan(n,"/",x)

{-
5. Утверждением будем называть логическую формулу, имеющую одну из следующих форм:
• p & q
• p | q
• ~p
где p и q — утверждения. Например, утверждениями являются следующие формулы:
• x
• x | y
• x & (x | ~y)
Разработайте тип данных Prop, представляющий утверждения такого вида. Определите следующие функции:
  1) vars :: Prop -> [String], которая возвращает список
    имен переменных (без повторений), встречающихся в утверждениях.
  2) Пусть задан список имен переменных
    и их значений типа Bool, например
    [("x",True),("y",False)]. Определите функцию
    truthValue :: Prop -> [(String,Bool)] -> Bool
    которая определяет, верно ли утверждение, если переменные имеют заданные списком значения.
  3) Определите функцию tautology :: Prop -> Bool, которая возвращает True, если утверждение 
    верно при любых значениях переменных, встречающихся в нем (например, это выполняется для утверждения (x | ~x)).
-}
data Prop = Var String | And Prop Prop | Or Prop Prop | Not Prop deriving (Eq,Show)
 
vars' :: Prop -> [String] -> [String]
vars' (Var x) acc = if x `elem` acc then acc  else x : acc
vars' (And x y) acc = vars' x (vars' y acc)
vars' (Or x y) acc = vars' x (vars' y acc)
vars' (Not x) acc = vars' x acc

vars :: Prop -> [String]
vars x = vars' x []

getVal :: String -> [(String,Bool)] -> Bool
getVal s vt = snd $ head w
              where w = filter (\ q -> s == fst q) vt  
 
calcValue :: Prop -> [(String,Bool)] -> Bool
calcValue (Var x) vt = getVal x vt
calcValue (And x y) vt = (calcValue x vt) && (calcValue y vt)
calcValue (Or x y) vt = (calcValue x vt) || (calcValue y vt)
calcValue (Not x) vt =  not (calcValue x vt)

{-
6.Лексические деревья (trie-деревья) используются для представления словарей. 
Каждый узел дерева содержит следующую информацию: символ, булевское значение и список поддеревьев 
(у каждого узла может быть произвольное количество дочерних деревьев).
-}
data LTree = Unit Char Bool [LTree] deriving (Eq,Show)
 
-- Извлечь из узла букву
 
getC :: LTree -> Char
getC (Unit c _ _) = c
 
-- найти букву в списке узлов
 
search :: Char -> [LTree] -> [LTree]
search c [] = []
search c (lt:lts) | ((getC lt) == c) = [lt]
                  | otherwise = search c lts
 
-- проверить существование слова

exist :: LTree -> String -> Bool
exist (Unit c f more) []     = False  -- строка кончилась, но без индикатора конца
exist (Unit c f _) [w]    = (c == w) && f  -- найден последний символ 
exist (Unit c f more) (w1:ws) | (w1 /= c)  = False -- очередной символ не совпадает
                              | (lm == []) = False  -- очередной символ не найден в продолжении
                              | otherwise  = exist (head lm) ws --  очередной символ найден в продолжении
                                where lm = search (head ws) more
{-
7. Теоретически возможно, хотя и неэффективно, определить целые числа с помощью 
рекурсивных типов данных следующим образом:
 data Number = Zero | Next Number Т. е. 
число является либо нулем (Zero), либо определяется, как число, следующее за предыдущим числом. 
Например, число 3 записывается как Next (Next (Next Zero)).
Определите для такого представления следующие функции:
1) fromInt, для заданного целого числа типа Integer возвращающую соответствующее ему значение типа Number.
2) toInt, преобразующую значение типа Number в соответствующее целое число.
3) plus :: Number -> Number -> Number, свои аргументы.
4) mult :: Number -> Number -> Number, умножающую свои аргументы.
5) dec, вычитающую единицу из своего аргумента. Для Zero функция должна возвращать Zero.
6) fact, вычисляющую факториал.
-}
data Number = Zero | Next Number deriving Show
 
dec (Next n) = n
dec _ = Zero
 
fromInt 0 = Zero
fromInt n = Next $ fromInt (n - 1)
 
toInt Zero = 0
toInt (Next n) = succ (toInt n) 
 
plus Zero n = n
plus t n = Next (plus (dec t) n) 
 
mult Zero _ = Zero
mult t n = plus n (mult (dec t) n)
 
fact Zero = Next Zero
fact n = mult n (fact (dec n))
--fact (Next$ Next$ Next$ Zero)

{-
8. Иерархия должностей в некоторой организации образует древовидную структуру. 
Каждый работник, однозначно характеризующийся уникальным именем, имеет несколько подчиненных. 
Определите тип данных, который представляет такую иерархию, и реализуйте следующие функции:

1. Функцию getSubordinate, возвращающую список подчиненных указанного работника.
2. Функцию getAllSubordinate, возвращающую список всех подчиненных данного работника, включая косвенных.
3. Функцию getBoss, возвращающую начальника указанного работника.
4. Функцию getList, возвращающую список пар, первым элементом которых является имя работника,
 а вторым - количество его подчиненных (включая косвенных).
-}
--import Data.List
--import Data.Maybe
 
data Worker = Worker {
    name_       :: String,
    workers_    :: [Worker]
}
 
findW root name = down root
    where down a
                | name_ a == name = [a]
                | otherwise = workers_ a >>= down
 
getBoss :: Worker -> String -> Maybe String
getBoss root name = listToMaybe$ down root
    where down a
            | any ((==name).name_) (workers_ a) = [name_ a]
            | otherwise = workers_ a >>= down
 
getSubordinates r n = findW r n >>= (map name_).workers_
 
getAllSubordinates :: Worker -> String -> [String]
getAllSubordinates r n = do
    e <- findW r n
    let s = (workers_ e >>= \w -> getAllSubordinates w (name_ w))
    map name_ (workers_ e) ++ s
 
getList :: Worker -> [(String,String)]
getList r = down (name_ r)
    where
        down n = [(n,w) | w <- getAllSubordinates r n] ++ (getSubordinates r n >>= down)
{-
9. Область на плоскости является либо прямоугольником, либо кругом, либо объединением областей, 
либо их пересечением. Прямоугольник характеризуется координатами левого нижнего и правого
верхнего углов, круг — координатами центра и радиусом. Разработайте структуру данных, 
представляющую область описанного вида. Определите следующие функции:

1) contains, проверяющая, что заданная точка попадает в область.

2) isRectangular, проверяющая, что область задается только прямоугольниками.

3) isEmpty, проверяющая, что область пуста, т. е. ни одна точкатплоскости не попадает в нее.
-}
type Point = (Int,Int) -- координаты точки
 
data Figura =  Empty
             | Rect Point Point -- левый нижний и правый верхний угол прямоугольника
             | Circle Point Int -- центр и радиус
             | Union Figura Figura
             | Intersection Figura Figura
        deriving Show
 
contains:: Point -> Figura -> Bool
contains (x0,y0) = go 
   where go Empty = False 
         go (Rect (l,b) (r,t)) = (l<=x0) && (r>=x0) && (b<=y0) && (t>=y0) 
         go (Circle (x,y) r) =((x-x0)^2 + (y-y0)^2)<=r^2
         go (Union f1 f2) = (go f1) || (go f2)
         go (Intersection f1 f2) = (go f1) && (go f2)
isRectangular:: Figura -> Bool
isRectangular (Rect _ _) = True
isRectangular (Union f1 f2) = isRectangular f1 && isRectangular f2
isRectangular (Intersection f1 f2) = isRectangular f1 && isRectangular f2
isRectangular _ = False

isEmpty:: Figura -> Bool
isEmpty f = f == Empty
{-
10. 
Класс в объектно-ориентированном языке содержит набор методов
(в данной работе будем игнорировать поля-данные класса). Кроме
того, он может иметь единственный родительский класс (не рас-
сматриваем случай множественного наследования). Однако суще-
ствуют классы и без родителей. При наследовании класса методы
предка добавляются к методам потомка. Определите тип данных,
представляющий информацию об иерархии классов. Опишите сле-
дующие функции:
1) getParent, возвращающую непосредственного предка класса
с указанным именем.
2) getPath, возвращающую список всех предков данного класса
(непосредственный предок, предок предка и т. д.)
3) getMethods, возвращающую список методов указанного
класса с учетом наследования.
4) inherit, добавляющая в иерархию классов класс с заданным
именем, унаследованый от указанного предка.
-}
data Class = Class { cid :: String, methods :: [String], subclasses :: [Class] } deriving Show

orelse = (++)
getParent hs cid1 = aux [] hs
  where aux s x 
          | cid x /= cid1 = foldr (orelse . aux [x])   [] (subclasses x)    
        aux s _           = s            

getPath hs cid1 = aux [] hs
  where aux s x 
          | cid x /= cid1 = foldr (orelse . aux (x:s)) [] (subclasses x)
        aux s _           = s      

inherit hs cid1 cl =  aux hs
  where aux hs' = hs' { subclasses = (if cid hs' == cid1 then (mixin hs' :) 
                                                         else (map aux)
                                     ) $ subclasses hs' 
                      }
        mixin hs' = cl { methods = methods cl ++ methods hs' }

getMethods hs cid1 = aux hs
  where aux x 
          | cid x /= cid1 = foldr (orelse . aux) [] (subclasses x)
        aux x             = [methods x] 
            
mkClass id' methods' = Class id' methods' []


