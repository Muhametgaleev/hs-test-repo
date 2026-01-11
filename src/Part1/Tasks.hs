module Part1.Tasks where

import Util(notImplementedYet)

normalizeAngle :: Double -> Double
normalizeAngle x = let tmp = x - 2 * pi * fromIntegral (floor (x / (2 * pi)))
                   in if tmp > pi then tmp - 2 * pi else if tmp < -pi then tmp + 2 * pi else tmp

factorial :: Int -> Double
factorial 0 = 1.0
factorial n = fromIntegral (product [1..n])

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin x = sinTaylor (normalizeAngle x) 0 0.0
  where
    sinTaylor x n acc
      | abs term < 1e-10 = acc
      | otherwise = sinTaylor x (n + 1) (acc + term)
      where term = let s = if even n then 1 else -1
                       p = x^(2*n+1)
                       f = factorial (2*n+1)
                   in s * p / f

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos x = cosTaylor (normalizeAngle x) 0 0.0
  where
    cosTaylor x n acc
      | abs term < 1e-10 = acc
      | otherwise = cosTaylor x (n + 1) (acc + term)
      where term = let s = if even n then 1 else -1
                       p = x^(2*n)
                       f = factorial (2*n)
                   in s * p / f

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD a 0 = abs a
myGCD a b = myGCD b (a `mod` b)

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year
  | month < 1 || month > 12 = False
  | day < 1 = False
  | month == 2 = day <= (if isLeapYear year then 29 else 28)
  | month `elem` [4, 6, 9, 11] = day <= 30
  | otherwise = day <= 31
  where
    isLeapYear y = (y `mod` 4 == 0 && y `mod` 100 /= 0) || (y `mod` 400 == 0)

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow _ 0 = 1
myPow x 1 = x
myPow x n
  | n < 0 = error "Negative exponent not supported"
  | even n = let h = myPow x (n `div` 2) in h * h
  | otherwise = x * myPow x (n - 1)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime n
  | n < 2 = False
  | n == 2 = True
  | even n = False
  | otherwise = checkDivisors n 3
  where
    checkDivisors n d
      | d * d > n = True
      | n `mod` d == 0 = False
      | otherwise = checkDivisors n (d + 2)

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea [] = 0
shapeArea [_] = 0
shapeArea points = abs (sh points) / 2.0
  where
    sh ps = sum [x1 * y2 - x2 * y1 | ((x1, y1), (x2, y2)) <- zip ps (tail ps ++ [head ps])]

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c
  | not (isTriangle a b c) = -1
  | isRight a b c = 2
  | isObtuse a b c = 0
  | otherwise = 1
  where
    isTriangle a b c = a + b > c && a + c > b && b + c > a
    isRight a b c = 
      let s = sort3 a b c
          a1 = s !! 0
          a2 = s !! 1
          a3 = s !! 2
      in abs (a1*a1 + a2*a2 - a3*a3) < 1e-10
    isObtuse a b c =
      let s = sort3 a b c
          a1 = s !! 0
          a2 = s !! 1
          a3 = s !! 2
      in a1*a1 + a2*a2 < a3*a3
    sort3 x y z = [min3 x y z, mid3 x y z, max3 x y z]
    min3 x y z = min x (min y z)
    max3 x y z = max x (max y z)
    mid3 x y z = x + y + z - min3 x y z - max3 x y z
