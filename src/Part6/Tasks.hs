{-# LANGUAGE FlexibleInstances #-}
module Part6.Tasks where

import Util (notImplementedYet)
import Data.Map

-- Разреженное представление матрицы. Все элементы, которых нет в sparseMatrixElements, считаются нулями
data SparseMatrix a = SparseMatrix {
                                sparseMatrixWidth :: Int,
                                sparseMatrixHeight :: Int,
                                sparseMatrixElements :: Map (Int, Int) a
                         } deriving (Show, Eq)

-- Определите класс типов "Матрица" с необходимыми (как вам кажется) операциями,
-- которые нужны, чтобы реализовать функции, представленные ниже
class Matrix mx where
    getWidth :: mx -> Int
    getHeight :: mx -> Int
    getElement :: mx -> Int -> Int -> Int
    createMatrix :: Int -> Int -> (Int -> Int -> Int) -> mx

-- Определите экземпляры данного класса для:
--  * числа (считается матрицей 1x1)
--  * списка списков чисел
--  * типа SparseMatrix, представленного выше
instance Matrix Int where
    getWidth _ = 1
    getHeight _ = 1
    getElement x 0 0 = x
    getElement _ _ _ = 0
    createMatrix _ _ f = f 0 0

instance Matrix [[Int]] where
    getWidth [] = 0
    getWidth (r:_) = length r
    getHeight = length
    getElement m r c
        | r < 0 || c < 0 = 0
        | r >= length m = 0
        | c >= length (m !! r) = 0
        | otherwise = (m !! r) !! c
    createMatrix w h f = [[f i j | j <- [0..w-1]] | i <- [0..h-1]]

instance Matrix (SparseMatrix Int) where
    getWidth = sparseMatrixWidth
    getHeight = sparseMatrixHeight
    getElement m r c = 
        case Data.Map.lookup (r, c) (sparseMatrixElements m) of
            Just v -> v
            Nothing -> 0
    createMatrix w h f = 
        let els = Data.Map.fromList [((i, j), f i j) | i <- [0..h-1], j <- [0..w-1], f i j /= 0]
        in SparseMatrix w h els

-- Реализуйте следующие функции
-- Единичная матрица
eye :: Matrix m => Int -> m
eye w = createMatrix w w (\i j -> if i == j then 1 else 0)

-- Матрица, заполненная нулями
zero :: Matrix m => Int -> Int -> m
zero w h = createMatrix w h (\_ _ -> 0)

-- Перемножение матриц
multiplyMatrix :: Matrix m => m -> m -> m
multiplyMatrix a b
    | getWidth a /= getHeight b = error "Matrix dimensions incompatible"
    | otherwise = createMatrix (getWidth b) (getHeight a) (\i j ->
        sum [getElement a i k * getElement b k j | k <- [0..getWidth a - 1]])

-- Определитель матрицы
determinant :: Matrix m => m -> Int
determinant m
    | getWidth m /= getHeight m = error "Determinant only for square matrices"
    | otherwise = det (getWidth m) m
  where
    det 1 m = getElement m 0 0
    det n m = sum [(-1)^j * getElement m 0 j * det (n-1) (minor m 0 j) | j <- [0..n-1]]
    minor m row col = createMatrix (getWidth m - 1) (getHeight m - 1) $ \i j ->
        getElement m (if i < row then i else i + 1) (if j < col then j else j + 1)
