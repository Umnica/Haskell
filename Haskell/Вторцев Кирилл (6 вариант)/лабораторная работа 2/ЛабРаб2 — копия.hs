{-�1
�������, ����������� �� ���� ����� ����� n � ������������ ������, ���������� n
���������, ������������� �� �����������
1)������ ����������� �����
2)������ �������� ����������� ����� 
3) ������ ��������� ����������� �����
4) ������ �����������
5) ������ ������� ������
7) ������ ���������� �����
8) ������ ������������� �����-}

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
{-�2 ���������� ��������� �������-}
-- �������, ����������� �� ����� ������ ������������ ����� � ����������� �� �������������� �������.
quantity :: [Double] -> Double
quantity [] = 0.0
quantity [x] = 1.0
quantity (x:xs) = 1.0 + (quantity xs)

sum_ :: [Double] -> Double
sum_ [] = 0.0
sum_ [x] = 1.0
sum_ (x:xs) = ((x) + (sum_ xs))

f9 n = (sum_ n) / (quantity n)

-- ������� 2 ���������� n-�� �������� �� ��������� ������ 
f10 :: Integer -> [Integer] -> [Integer]
f10 n (x:xs) = if n == 1 then xs  else x: f10 (n-1) xs 

{-������� �������� ��������� ���� �������. ���������� ������, ����������� �� ���� ��������� 
�������-���������� ������, ��� ���������� ������ ����� ���� ������ ����� -}
add2List :: [Integer] -> [Integer] -> [Integer]
add2List [][] = []
add2List [](n1:nx) =(n1:nx) --���� �����
add2List (l1:lx)[] = (l1:lx)
add2List (l1:lx) (n1:nx) = (l1+n1): (add2List lx nx)
{-������� ������������ ������� �������� ������ � �������� �������� � �������� ������
[1,2,3,...,n]-}
f11 :: [Integer] -> [Integer]
f11 [] = []
f11 [x] = [x]
f11 (x:y:xs) = y:x:(f11 xs)

--������� twopow n
twopow :: Integer -> Integer
twopow n | (n == 1) = 2
         | (even n) = w * w
         | otherwise = (w * w) + (w * w) where w = twopow (n `div` 2)

{-������� removeOdd, ������� ������� �� ��������� ������ ����� ����� ��� �������� �����.
��������: removeOdd [1,4,5,6,10] ������ ���������� [4,10]-}
removeOdd :: [Integer] -> [Integer]
removeOdd [] = []
removeOdd [x] = if odd x == True then [] else [x]
removeOdd (x:xs) = if odd x == True then removeOdd xs else x:removeOdd xs

{-������� removeEmpty, ������� ������� ������ ������ �� ��-
������� ������ �����. 
��������: removeEmpty ["", "Hello", "", "", "World!"] ���������� ["Hello","World!"].-}
removeEmpty :: [String]->[String]
removeEmpty [] = []
removeEmpty [x] = if x == "" then [] else [x]
removeEmpty (x:xs) = if x == "" then removeEmpty xs else x:removeEmpty xs

-- ������� countTrue :: [Bool] -> Integer, ������������ ���������� ��������� ������, ������ True.
countTrue :: [Bool] -> Integer
countTrue [] = 0
countTrue [x] = if x == True then 1 else 0
countTrue (x:xs) = if x == True then 1 + (countTrue xs) else countTrue xs

--makePositive ������ ����� - �� +
makePositive :: [Integer]->[Integer]
makePositive [x] = if x < 0 then [(-1)*x] else [x]
makePositive (x:xs) = if x < 0 then ((-1)*x): (makePositive xs) else x: (makePositive xs)

{-������� delete :: Char -> String -> String, ������� ��������� �� ���� �����, � ������� 
������� ��� �������� �������.
������: delete 'l' "Hello world!" ������ ���������� "Heo word!"-}
delete :: Char -> String -> String
delete c [] = "" 
delete c cs = if c == (head cs) then delete c (tail cs) else (head cs): (delete c (tail cs))

-- �������� ���� ������ � ������ �� ������ 
substitute :: Char->Char->String->String
substitute c1 c2 [] = ""
substitute c1 c2 str = if c1 == (head str) then c2:substitute c1 c2 (tail str) else (head str): (substitute c1 c2 (tail str))
--� �� ������ ������� ���������� ������������ ������, ������� ������ ��� ��������. ��� ������� 6,11, �.�. ��� 6

 
