module Part3.Tasks where

import Util (notImplementedYet)
import Data.List (maximumBy)

-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
finc f n = f n : finc f (n + 1)

-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ff :: (a -> a) -> a -> [a]
ff f x = x : ff f (f x)

-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах (если таковых несколько -- вернуть любую)
mostFreq :: [Int] -> Int
mostFreq xs = fst (maximumBy cc dc)
  where
    ds = concatMap (do' . abs) xs
    do' 0 = [0]
    do' n = do'' n []
    do'' 0 acc = acc
    do'' n acc = do'' (n `div` 10) (n `mod` 10 : acc)
    dc = cd ds
    cd ds = [(d, length (filter (== d) ds)) | d <- [0..9]]
    cc (_, c1) (_, c2) = compare c1 c2

-- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq (x:xs) = x : uniq (filter (/= x) xs)

-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.
grokBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
grokBy f l = grokBy' f l []
  where
    grokBy' _ [] acc = acc
    grokBy' f (x:xs) acc =
      let k = f x
          (fnd, r) = part ((== k) . fst) acc
      in if null fnd
         then grokBy' f xs ((k, [x]) : r)
         else grokBy' f xs ((k, x : snd (head fnd)) : r)
    part _ [] = ([], [])
    part p (x:xs) =
      let (y, n) = part p xs
      in if p x then (x:y, n) else (y, x:n)
