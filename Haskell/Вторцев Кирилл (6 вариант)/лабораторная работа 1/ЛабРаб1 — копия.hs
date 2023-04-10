--Найдем максимум из 3-х чисел
max3 :: Int -> Int -> Int -> Int
max3 x y z = max ( max x y ) z

--Найдем минимум из 3-х чисел
min3 :: Int -> Int -> Int -> Int
min3 x y z = min ( min x y ) z

--Функция sort2
sort2 :: Int -> Int -> ( Int, Int )
sort2 x y = (min x y, max x y)

--Функция bothTrue1
bothTrue1 :: Bool -> Bool -> Bool
bothTrue1 a b = min a b

--Функция bothTrue2
bothTrue2 :: Bool -> Bool -> Bool
bothTrue2 True True = True
bothTrue2 True False = False
bothTrue2 False True = False
bothTrue2 False False = False

--Функция bothTrue3
bothTrue3 :: Bool -> Bool -> Bool
bothTrue3 True x = x
bothTrue3 _ _ = False

--Функция solve2
solve2 :: Double -> Double -> (Bool, Double)
solve2 a b = if a == 0 then ( False, 0.0) else ( True, ((-b)/a))


--Функция solve22
solve22 :: Double -> Double -> (Bool, Double)
solve22 0 _ = (False, 0.0)
solve22 a b = (True, ((-b)/a))

--Функция isParallel
isParallel :: (Double, Double) -> (Double, Double) -> (Double, Double) -> (Double, Double) -> Bool
isParallel (a, b) (c, d) (e, f) (g, h) = (c - a) / (g - e) == (d - b) / (h - f)

--функция isIncluded
isIncluded :: (Double, Double) -> Double -> (Double, Double) -> Double -> Bool
isIncluded (x', y') a (x'', y'') b = sqrt((x''-x')^2 + (y''-y')^2) + b <= a

--функция isRectangular
isRectangular :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Bool
isRectangular (x', y') (x'', y'') (x''', y''') = (x'' - x') * (x''' - x') + (y'' - y') * (y''' - y') == 0 || (x' - x'') * (x''' - x'') + (y' - y'') * (y''' - y'') == 0 || (x' - x''')*(x'' - x''') + (y' - y''') * (y'' - y''') == 0

--функция isTriangle
isTriangle :: Double -> Double -> Double -> Bool
isTriangle x y z = x^2 + y^2 == z^2 || x^2 + z^2 == y^2 || y^2 + z^2 == x^2

--функция isSorted
isSorted :: Double -> Double -> Double -> Bool
isSorted a b c = a >= b && b >= c || a <= b && a <= c

fnat :: Int->[Int]
fnat 0 = []
fnat n = if n<0 then fnat(n+1)++[n] else fnat(n-1)++[n]

fnat1 :: Int -> [Int]
fnat1 0 = []
fnat1 n = fnat(n-1)++[n]