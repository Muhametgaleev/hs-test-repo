module Part4.Tasks where

import Util(notImplementedYet)

-- Перевёрнутый связный список -- хранит ссылку не на последующию, а на предыдущую ячейку
data ReverseList a = REmpty | (ReverseList a) :< a
infixl 5 :<

-- Функция-пример, делает из перевёрнутого списка обычный список
-- Использовать rlistToList в реализации классов запрещено =)
rlistToList :: ReverseList a -> [a]
rlistToList lst =
    reverse (reversed lst)
    where reversed REmpty = []
          reversed (init :< last) = last : reversed init

-- Реализуйте обратное преобразование
listToRlist :: [a] -> ReverseList a
listToRlist [] = REmpty
listToRlist (x:xs) = listToRlist xs :< x

-- Реализуйте все представленные ниже классы (см. тесты)
instance Show a => Show (ReverseList a) where
    showsPrec _ REmpty = showString "[]"
    showsPrec _ lst = showString "[" . si lst . showString "]"
      where
        si REmpty = id
        si (a :< b) = si a . (if isREmpty a then id else showString ",") . shows b
        isREmpty REmpty = True
        isREmpty _ = False
    show REmpty = "[]"
    show lst = "[" ++ si2 lst ++ "]"
      where
        si2 REmpty = ""
        si2 (a :< b) = si2 a ++ sc2 a ++ show b
        sc2 REmpty = ""
        sc2 _ = ","

instance Eq a => Eq (ReverseList a) where
    REmpty == REmpty = True
    REmpty == _ = False
    _ == REmpty = False
    (a :< b) == (c :< d) = b == d && a == c

instance Semigroup (ReverseList a) where
    REmpty <> b = b
    (a :< x) <> b = (a <> b) :< x

instance Monoid (ReverseList a) where
    mempty = REmpty

instance Functor ReverseList where
    fmap _ REmpty = REmpty
    fmap f (a :< b) = fmap f a :< f b

instance Applicative ReverseList where
    pure x = REmpty :< x
    REmpty <*> _ = REmpty
    _ <*> REmpty = REmpty
    (a :< f) <*> b = (a <*> b) <> fmap f b

instance Monad ReverseList where
    return = pure
    REmpty >>= _ = REmpty
    (a :< b) >>= f = (a >>= f) <> f b
