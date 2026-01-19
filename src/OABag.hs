{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OABag
  ( OABag,
    empty,
    insert,
    remove,
    size,
    toList,
    fromList,
    count,
    filterBag,
    mapBag,
    foldlBag,
    foldrBag,
  )
where

import Data.Hashable (Hashable, hash)
import qualified Data.List as L
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Prelude hiding (lookup)

-- | Тип ячейки в хеш-таблице.
data Slot a
  = Free
  | Occupied a Int
  | Deleted
  deriving (Show, Eq, Generic)

-- | Структура данных Bag (Multiset) на основе Open Addressing.
data OABag a = OABag
  { capacity :: Int,
    load :: Int,
    size :: Int,
    table :: V.Vector (Slot a)
  }
  deriving (Show, Generic)

-- | Константы
initialCapacity :: Int
initialCapacity = 16

maxLoadFactor :: Double
maxLoadFactor = 0.7

-- | Создание пустого Bag
empty :: OABag a
empty =
  OABag
    { capacity = initialCapacity,
      load = 0,
      size = 0,
      table = V.replicate initialCapacity Free
    }

-- | Хелпер: получение индекса
idx :: (Hashable a) => a -> Int -> Int
idx key cap = abs (hash key) `rem` cap

-- | Получение количества вхождений элемента
count :: (Hashable a, Eq a) => a -> OABag a -> Int
count key bag = go (idx key (capacity bag)) 0
  where
    cap = capacity bag
    vec = table bag
    go i attempts
      | attempts >= cap = 0
      | otherwise = case vec V.! i of
          Free -> 0
          Deleted -> go ((i + 1) `rem` cap) (attempts + 1)
          Occupied k c -> if k == key then c else go ((i + 1) `rem` cap) (attempts + 1)

-- | Вставка элемента
insert :: (Hashable a, Eq a) => a -> OABag a -> OABag a
insert key bag
  | fromIntegral (load bag + 1) > fromIntegral (capacity bag) * maxLoadFactor =
      insert key (resize bag)
  | otherwise =
      let cap = capacity bag
          startI = idx key cap

          findSlot i attempts
            | attempts >= cap = error "OABag full (should have resized)"
            | otherwise = case table bag V.! i of
                Occupied k c
                  | k == key ->
                      bag
                        { size = size bag + 1,
                          table = V.update (table bag) (V.singleton (i, Occupied key (c + 1)))
                        }
                Free ->
                  bag
                    { load = load bag + 1,
                      size = size bag + 1,
                      table = V.update (table bag) (V.singleton (i, Occupied key 1))
                    }
                Deleted ->
                  findSlotWithDeleted i attempts i
                Occupied _ _ ->
                  findSlot ((i + 1) `rem` cap) (attempts + 1)

          findSlotWithDeleted i attempts firstDelIdx
            | attempts >= cap = insertAtDeleted firstDelIdx
            | otherwise = case table bag V.! i of
                Free -> insertAtDeleted firstDelIdx
                Occupied k c
                  | k == key ->
                      bag
                        { size = size bag + 1,
                          table = V.update (table bag) (V.singleton (i, Occupied key (c + 1)))
                        }
                _ -> findSlotWithDeleted ((i + 1) `rem` cap) (attempts + 1) firstDelIdx

          insertAtDeleted i =
            bag
              { load = load bag,
                size = size bag + 1,
                table = V.update (table bag) (V.singleton (i, Occupied key 1))
              }
       in findSlot startI 0

-- | Удаление элемента
remove :: (Hashable a, Eq a) => a -> OABag a -> OABag a
remove key bag =
  let cap = capacity bag
      startI = idx key cap

      go i attempts
        | attempts >= cap = bag
        | otherwise = case table bag V.! i of
            Free -> bag
            Deleted -> go ((i + 1) `rem` cap) (attempts + 1)
            Occupied k c
              | k == key ->
                  if c > 1
                    then
                      bag
                        { size = size bag - 1,
                          table = V.update (table bag) (V.singleton (i, Occupied key (c - 1)))
                        }
                    else
                      bag
                        { size = size bag - 1,
                          table = V.update (table bag) (V.singleton (i, Deleted))
                        }
              | otherwise -> go ((i + 1) `rem` cap) (attempts + 1)
   in go startI 0

-- | Внутренний fold по слотам (исправленный для HLint)
foldrSlot :: (a -> Int -> b -> b) -> b -> V.Vector (Slot a) -> b
foldrSlot f = V.foldr step
  where
    step slot acc = case slot of
      Occupied k c -> f k c acc
      _ -> acc

-- | Увеличение размера таблицы и рехеширование
resize :: (Hashable a, Eq a) => OABag a -> OABag a
resize bag =
  let newCap = capacity bag * 2
      newTable = V.replicate newCap Free

      rawInsert vec (k, c) =
        let start = idx k newCap
            probe j = case vec V.! j of
              Free -> V.update vec (V.singleton (j, Occupied k c))
              _ -> probe ((j + 1) `rem` newCap)
         in probe start

      allElems = foldrSlot (\k c acc -> (k, c) : acc) [] (table bag)
      filledTable = L.foldl' rawInsert newTable allElems
   in OABag
        { capacity = newCap,
          load = length allElems,
          size = size bag,
          table = filledTable
        }

-- | Преобразование в список
toList :: OABag a -> [a]
toList bag = foldrSlot (\k c acc -> replicate c k ++ acc) [] (table bag)

-- | Создание из списка
fromList :: (Hashable a, Eq a) => [a] -> OABag a
fromList = L.foldl' (flip insert) empty

-- | Map (исправленный для HLint)
mapBag :: (Hashable a, Eq a, Hashable b, Eq b) => (a -> b) -> OABag a -> OABag b
mapBag f bag = foldrSlot (insertCount . f) empty (table bag)
  where
    insertCount k c b = iterate (insert k) b !! c

-- | Filter (исправленный: удален мертвый код)
filterBag :: (Hashable a, Eq a) => (a -> Bool) -> OABag a -> OABag a
filterBag predicate bag =
  fromList $ filter predicate $ toList bag

-- | Свертки
foldrBag :: (a -> b -> b) -> b -> OABag a -> b
foldrBag f z bag = foldr f z (toList bag)

foldlBag :: (b -> a -> b) -> b -> OABag a -> b
foldlBag f z bag = L.foldl' f z (toList bag)

-- | Реализация Eq
instance (Hashable a, Eq a) => Eq (OABag a) where
  b1 == b2 =
    size b1 == size b2
      && subBag b1 b2
    where
      subBag bagA bagB =
        foldrSlot (\k c acc -> acc && count k bagB == c) True (table bagA)

-- | Реализация Semigroup и Monoid
instance (Hashable a, Eq a) => Semigroup (OABag a) where
  b1 <> b2 = foldrBag insert b2 b1

instance (Hashable a, Eq a) => Monoid (OABag a) where
  mempty = empty

-- | Реализация Foldable
instance Foldable OABag where
  foldr = foldrBag
  foldl = foldlBag