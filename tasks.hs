1-	Даны два списка чисел. Верните список чисел, присутствующих в обоих исходных списках.
Пример:
mergeLists [0,1,2,3,4] [4,2,40] -> [2,4]
mergeLists [0,1,2] [4,5] -> []
Solution:
mergeLists :: [Int]-> [Int]-> [Int]
mergeLists a [] = []
mergeLists [] a = []
mergeLists (h1:t1)(h2:t2) = takeWhile(==h1)(h2:t2) ++ mergeLists t1 (h2:t2)
Edition : if we wont to take all common elements with all iterations 
mergeLists1 [0,1,2,2,3,4] [4,2,40] -> [2,2,4]
mergeLists1 (h1:t1)(h2:t2) = filter (==h1)(h2:t2) ++ mergeLists1 t1 (h2:t2)
or: 
mergeLists1 (h1:t1)(h2:t2) = (\x -> x==h1 | x <-(h2:t2)) ++ mergeLists1 t1 (h2:t2)

2-	Дан список чисел ts и число x. Верните список чисел, в которых присутствует хотя бы одна такая же цифра, как в x. Систему счисления считать десятичной.
Пример:
sameDigits [1,11,45,23] 12 -> [1,11,23]
sameDigits [72,47,55] 7 -> [72, 47]
sameDigits :: [Int]-> Int-> [Int]
sameDigits (h:t) x = sameDigits’ ( show (h) show(x)) ++ sameDigits t x
 where 
 sameDigits’ :: [String]-> [String]-> [String]
 sameDigits’ (h:t) a = if elem h a then a sameDigits’ t a
                         else sameDigits’ t a


3-	Реализуйте функцию, которая принимает на вход список целых чисел и возвращает для каждого элемента его изменение относительно предыдущего в некотором представлении.
Пример:
diffs [1,2,3,2,2] -> [Plus 1, Plus 1, Minus 1, Equal]
diffs [1,32,32,1] -> [Plus 31, Equal, Equal, Minus 31]

data Comp = Equal String
           |Diff String Int
diffs :: [Int] -> [Comp]
diffs [] = error “Empty list”
diffs [x] = error “No compare”    -- or []
diffs (h:t)| h < (head(t)) = “Plus” (h–(head(t)) ++ diffs(t)
           | h > (head(t)) = “Minus” ((head(t))–h) ++ diffs(t)
           |h == (head(t)) = “Equal” ++ diffs(t)
Также необходимо реализовать требуемый для решения задачи тип данных.
4-	Постройте уравнение прямой, проходящей через две заданные точки в прямоугольной декартовой системе координат на плоскости.
Пример:
equline 3 2 2 6 -> "4x + 1y = 14"
equline :: Int -> Int -> Int -> Int -> String
equline x1 y1 x2 y2 = unword (equline’) where
                equline’ :: Int -> Int -> Int -> Int -> [String]
                equline’ = show (m) ++”x” ++ “ + y =” ++ show(g)
                       where m = (y2-y1)/(x2-x1)
                             g = y2 – m*x2


5-	Опишите тип данных для реализации троичной логики со значениями "Правда", "Ложь" и "Неизвестно". Реализуйте функции вычисления операций "не", "и" и "или" над этим типом.

data MyLogic = myTrue | myFalse | Neutral
orOp :: MyLogic -> MyLogic -> MyLogic
orOp myTrue myTrue = myTrue
orOp myTrue myFalse = myTrue
orOp myFalse myTrue = myTrue
orOp myFalse myFalse = myFalse
orOp _ _ = Neutral


andOp :: MyLogic -> MyLogic -> MyLogic 
andOp myTrue myTrue = myTrue
andOp myTrue myFalse = myFalse
andOp myFalse myTrue = myFalse
andOp myFalse myFalse = myFalse
andOp _ _ = Neutral


notOp :: MyLogic -> MyLogic
notOp myTrue = myFalse
notOp myFalse = myTrue
notOp _ = Neutral





6-	Дана пара отсортированных списков чисел lst1 и lst2. Необходимо вернуть отсортированный список, образованный объединением lst1 и lst2. Функцию sort использовать нельзя.
mySort:: [Int]-> [Int]-> [Int]
mySort a [] = a
mySort [] a = a
mySort (h:t) lst |notElem (h) lst = h++ lst ++ mySort t lst
                   otherwise mySort t1 lst

7-	Реализуйте тип данных, позволяющий задавать команды управления лентой вида "Влево на определённое число шагов", "Вправо на определённое число шагов", "Запись". Реализуйте функцию, которая принимает на вход список команд и начальную позицию и возвращает список позиций, на которых происходила запись.
Give me an example please , I didn’t understand
data Shifting = Lshift Int | Rshift Int | Record :: Int
   myRec:: [Shifting]-> Int-> [Int]

8-	Дан список чисел. Необходимо посчитать число чётных и нечётных чисел в этом списке.
countNum :: [Int]-> (Int , Int)
countNum [] = error “Empty List”
 countNum a = ( length.filter (odd) a , length.filter (even) a)




9-	Дан список чисел и функция f :: Int -> Int -> Int. Необходимо найти такую пару чисел (a,b) в списке, что f a b максимально.
maxVal :: [Int]->( Int -> Int -> Int) -> (Int , Int)
maxVal [] f = error “Error”
maxVal (h:t) f |length (h:t) =< 2 = error”Short list”
               | length (h:t) == 2 = (h ,head t)
               | length (h:t) >= 2 = maxVal’ (h:t) f
 Where 
maxVal’ (h:t) f = if f h (haed t) >= f (haed t) (1 !! t) then (h ,head t) maxVal’ t f
                   else (head t , 1 !! t ) maxVal’ t f

10-	Дан список чисел, число n и функция f :: Int -> Int -> Int. Необходимо найти любую такую пару чисел (a,b) в списке, что f a b равно n.
equVal :: [Int]->( Int -> Int -> Int) -> Int -> (Int , Int)
equVal [] _ _ = error “Error”
equVal [x] _ _  = error “Error”
equVal (h:t) f n |f h (head(t))== n then (h,head (t))
                 otherwise equVal t f n  

11-	Даны два списка чисел, a и b. Нужно посчитать произведение всех попарных сумм элементов из a и b.
If it is intended to obtain the sum of the sum of the corresponding elements from the two entries:
sumPare :: [Int]-> [Int]-> Int
sumPare [] [] = error “Empty lists”
sumPare a [] = a
sumPare [] b = b
sumPare (h1:t1) (h2:t2) |length(h1:t1) == length(h2:t2) = sum.sumPare’(h1:t1)(h2:t2)
                        |length(h1:t1)<= length(h2:t2) = sum.sumPare’(h1:t1)(h2:t2)’
                        |length(h1:t1)>= length(h2:t2)= sum.sumPare’ (h1:t1)’(h2:t2)
Where 
   (h1:t1)’ = take (length (h2:t2)) (h1:t1)
   (h2:t2)’ = take (length (h1:t1)) (h2:t2)
   sumPare’ :: [Int]-> [Int]-> [Int]
   sumPare’ (h1:t1) (h2:t2) = h1 +h2 ++ sumPare’ t1 t2
Edition : If we wanted to get a list containing the totals of the items corresponding to the two entered lists :
sumPare :: [Int]-> [Int]-> [Int]
sumPare [] [] = error “Empty lists”
sumPare a [] = a
sumPare [] b = b
sumPare (h1:t1) (h2:t2) |length(h1:t1) == length(h2:t2) = sumPare’ (h1:t1) (h2:t2)
                        |length(h1:t1)<= length(h2:t2) = sumPare’(h1:t1)(h2:t2)’
                        |length(h1:t1)>= length(h2:t2)= sumPare’ (h1:t1)’(h2:t2)
Where 
   (h1:t1)’ = take (length (h2:t2)) (h1:t1)
   (h2:t2)’ = take (length (h1:t1)) (h2:t2)
   sumPare’ :: [Int]-> [Int]-> [Int]
   sumPare’ (h1:t1) (h2:t2) = h1 +h2 ++ sumPare’ t1 t2

12-	Дано число n и функция f :: a -> a. Необходимо вернуть функцию, которая применяет f к своему аргументу n раз.
funImp :: Int-> (a -> a) -> (a -> a)
funImp 0 f = error “Error”
funImp 1 f = f
funImp n f = f . funImp (n-1) f

13-	Дан список функций fs :: [Int -> Int] и число n. Необходимо применить все функции из fs к n в порядке встречаемости в fs.
impFunList :: [(Int-> Int)]-> Int-> [Int]
impFunList [] _ = error “Empty List”
impFunList (f:fs) n = map (impFunList’) (f:fs)
   where 
   impFunList’ :: (Int-> Int) -> Int -> Int
     impFunList’ f n = f $ n
