{-№1
Функция, принимающая на вход целое число n и возвращающее список, содержащий n
элементов, упорядоченных по возрастанию
1)Список натуральных чисел
2)Список нечетных натуральныз чисел 
3) Список квадратов натуральных чисел
4) Список факториалов
5) Список степени двойки
7) Список треугльных чисел
8) Список пирамидальных чисел-}

f1 :: Integer -> [Integer]
f1 0 = []
f1 n = f1(n - 1) ++ (n : [])

f2 :: Integer -> [Integer]
f2 0 = []
f2 n = f2(n - 1) ++ (2 * n - 1 : [])

f3 :: Integer -> [Integer]
f3 0 = []
f3 n = f3(n - 1) ++ (2 * n : [])

f4 :: Integer -> [Integer]
f4 0 = []
f4 n = f4(n - 1) ++ (n * n : [])

f5 :: Integer -> [Integer]
factorial 0 = 1
factorial n = n * factorial(n - 1)
f5 0 = []
f5 n = f5(n - 1) ++ [factorial(n)]

f6 :: Integer -> [Integer]
f6 0 = []
f6 n = f6(n - 1) ++ (2^n : [])

f7 :: Integer -> [Integer]
t 1 = 1
t(n) = n + t(n - 1)
f7 0 = []
f7 n = f7(n - 1) ++ [t(n)]

f8 :: Integer -> [Integer]
p1 = 1
p(n) = t(n) + p(n-1)
f8 0 = []
f8 n = f8(n -1) ++ [p(n)]
{-№2 Определить следующие функции-}
-- Функция, принимающая на входе список вещественных чисел и вычисляющую их арифметическое среднее.
quantity :: [Double] -> Double
quantity [] = 0.0
quantity [x] = 1.0
quantity (x:xs) = 1.0 + (quantity xs)

sum_ :: [Double] -> Double
sum_ [] = 0.0
sum_ [x] = 1.0
sum_ (x:xs) = ((x) + (sum_ xs))

f9 n = (sum_ n) / (quantity n)

-- Функция 2 вычленения n-го элемента из заданного списка 
f10 :: Integer -> [Integer] -> [Integer]
f10 n (x:xs) = if n == 1 then xs  else x: f10 (n-1) xs 

{-Функция сложения элементов двух списков. Возвращает список, составленый из сумм элементов 
списков-параметров Учесть, что переданные списки могут быть разной длины -}
add2List :: [Integer] -> [Integer] -> [Integer]
add2List [][] = []
add2List [](n1:nx) =(n1:nx) --учёт длины
add2List (l1:lx)[] = (l1:lx)
add2List (l1:lx) (n1:nx) = (l1+n1): (add2List lx nx)
{-Функция перестановки местами соседних четных и нечетных элеметов в заданном списке
[1,2,3,...,n]-}
f11 :: [Integer] -> [Integer]
f11 [] = []
f11 [x] = [x]
f11 (x:y:xs) = y:x:(f11 xs)

--Функция twopow n
twopow :: Integer -> Integer
twopow n | (n == 1) = 2
         | (even n) = w * w
         | otherwise = (w * w) + (w * w) where w = twopow (n `div` 2)

{-Функция removeOdd, которая удаляет из заданного списка целые числа все нечетные числа.
Например: removeOdd [1,4,5,6,10] должен возвращать [4,10]-}
removeOdd :: [Integer] -> [Integer]
removeOdd [] = []
removeOdd [x] = if odd x == True then [] else [x]
removeOdd (x:xs) = if odd x == True then removeOdd xs else x:removeOdd xs

{-Функция removeEmpty, которая удаляет пустые строки из за-
данного списка строк. 
Например: removeEmpty ["", "Hello", "", "", "World!"] возвращает ["Hello","World!"].-}
removeEmpty :: [String]->[String]
removeEmpty [] = []
removeEmpty [x] = if x == "" then [] else [x]
removeEmpty (x:xs) = if x == "" then removeEmpty xs else x:removeEmpty xs

-- Функция countTrue :: [Bool] -> Integer, возвращающая количество элементов списка, равных True.
countTrue :: [Bool] -> Integer
countTrue [] = 0
countTrue [x] = if x == True then 1 else 0
countTrue (x:xs) = if x == True then 1 + (countTrue xs) else countTrue xs

--makePositive меняет знаки - на +
makePositive :: [Integer]->[Integer]
makePositive [x] = if x < 0 then [(-1)*x] else [x]
makePositive (x:xs) = if x < 0 then ((-1)*x): (makePositive xs) else x: (makePositive xs)

{-Функция delete :: Char -> String -> String, которая принимает на вход стоку, в которой 
удалены все взожения символа.
Пример: delete 'l' "Hello world!" должно возвращать "Heo word!"-}
delete :: Char -> String -> String
delete c [] = "" 
delete c cs = if c == (head cs) then delete c (tail cs) else (head cs): (delete c (tail cs))

-- Заменить один символ в строке на другой 
substitute :: Char->Char->String->String
substitute c1 c2 [] = ""
substitute c1 c2 str = if c1 == (head str) then c2:substitute c1 c2 (tail str) else (head str): (substitute c1 c2 (tail str))
--Я не увидел порядок выполнения лаюораторной работы, поэтому сделал все варианты. Мое задание 6,11, т.к. вар 6

 
