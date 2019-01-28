module Task1_2 where

import Todo(todo)

-- синус числа (формула Тейлора)
sin :: Double -> Double
sin x = map (exp^(x*sqrt(-1))-exp^(x*(-sqrt(-1)))/(2*sqrt(-1))[1..]

-- косинус числа (формула Тейлора)
cos :: Double -> Double
cos x = map (exp^(x*sqrt(-1))+exp^(x*(-sqrt(-1)))/2)[1..]

-- наибольший общий делитель двух чисел
gcd :: Integer -> Integer -> Integer
gcd x 0 = x
gcd x y = gcd y (x `mod` y)

-- существует ли полный целочисленный квадрат в диапазоне [from, to)?
doesSquareBetweenExist :: Integer -> Integer -> Bool
doesSquareBetweenExist from to = todo

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year = 

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
pow :: Integer -> Integer -> Integer
pow x y = todo

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime x = todo

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points = todo

-- треугольник задан своими координатами.
-- функция должна вернуть 
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Point2D -> Point2D -> Point2D -> Integer
triangleKind a b c = todo
