# Отчет по лабораторной работе №2
## "Функциональное программирование"

**Вариант:** oa-bag (Open Addressing Hashmap-based Bag)
**Язык:** Haskell

**Выполнил:** Студент группы P3312 Лобов Максим Павлович

---

### 1. Требования к разработанному ПО

Целью работы является освоение построения пользовательских типов данных, полиморфизма и тестирования в функциональном стиле.

**Функциональные требования:**
1.  Реализовать структуру данных **Bag** (Мультимножество) на основе хеш-таблицы с **открытой адресацией** (Open Addressing).
2.  Поддержать операции:
    *   добавление (`insert`) и удаление (`remove`) элементов;
    *   фильтрация (`filter`);
    *   отображение (`map`);
    *   свертки (`foldl`, `foldr`);
    *   получение количества вхождений (`count`).
3.  Структура данных должна быть неизменяемой (Immutable).
4.  Структура должна реализовывать интерфейсы `Semigroup` и `Monoid`.
5.  Реализовать полиморфизм (структура должна работать с любыми типами данных, поддерживающими хеширование и сравнение).

**Нефункциональные требования:**
1.  Использование идиоматичного стиля Haskell.
2.  Покрытие кода Unit-тестами (`hspec`).
3.  Покрытие кода Property-based тестами (`QuickCheck`), включая проверку свойств моноида.
4.  Отсутствие предупреждений компилятора (`-Wall`, `-Werror`) и линтера (`HLint`).
5.  Настройка CI (GitHub Actions).

---

### 2. Ключевые элементы реализации

В качестве основы для хранения данных использован неизменяемый `Data.Vector`. Для разрешения коллизий используется метод линейного пробирования (Linear Probing).

#### 2.1 Структура данных и типы
Используется ADT (Algebraic Data Type) `Slot` для поддержки механики "Tombstone" (удаленных ячеек), необходимой для корректной работы линейного поиска.

```haskell
-- Тип ячейки хеш-таблицы
data Slot a
  = Free              -- Свободно
  | Occupied a Int    -- Занято (элемент и счетчик вхождений)
  | Deleted           -- Удалено (Tombstone)
  deriving (Show, Eq, Generic)

-- Основная структура
data OABag a = OABag
  { capacity  :: Int
  , load      :: Int                  -- Кол-во занятых слотов (для ресайза)
  , totalSize :: Int                  -- Общее кол-во элементов (сумма счетчиков)
  , table     :: V.Vector (Slot a)    -- Неизменяемый вектор
  } deriving (Show, Generic)
```

#### 2.2 Вставка элемента (Insert)
Реализует неизменяемость через создание копии вектора при модификации.

```haskell
insert :: (Hashable a, Eq a) => a -> OABag a -> OABag a
insert key bag
  -- Проверка Load Factor и ресайз при необходимости
  | fromIntegral (load bag + 1) > fromIntegral (capacity bag) * maxLoadFactor =
      insert key (resize bag)
  | otherwise =
      let cap = capacity bag
          startI = idx key cap
          -- Линейное пробирование
          findSlot i attempts
            | attempts >= cap = error "Full"
            | otherwise = case table bag V.! i of
                -- Обновление существующего (копирование вектора с обновленной ячейкой)
                Occupied k c | k == key -> 
                    bag { totalSize = totalSize bag + 1
                        , table = V.update (table bag) (V.singleton (i, Occupied key (c + 1))) 
                        }
                -- Вставка в свободное место
                Free -> 
                    bag { load = load bag + 1
                        , totalSize = totalSize bag + 1
                        , table = V.update (table bag) (V.singleton (i, Occupied key 1)) 
                        }
                -- Логика пробирования Deleted/Occupied пропущена для краткости...
                _ -> findSlot ((i + 1) `rem` cap) (attempts + 1)
      in findSlot startI 0
```

#### 2.3 Свертка и классы типов
Реализация `Foldable` позволяет использовать стандартные функции (`toList`, `length`) "бесплатно".

```haskell
-- Вспомогательный фолд по внутреннему вектору
foldrSlot :: (a -> Int -> b -> b) -> b -> V.Vector (Slot a) -> b
foldrSlot f = V.foldr step
  where
    step slot acc = case slot of
        Occupied k c -> f k c acc -- Передаем элемент и его кол-во
        _            -> acc

instance Foldable OABag where
    foldr f z bag = foldr f z (toList bag)
    foldl f z bag = L.foldl' f z (toList bag)

instance (Hashable a, Eq a) => Monoid (OABag a) where
    mempty = empty
```

---

### 3. Тестирование

Для тестирования использовались библиотеки `Hspec` (Unit-тесты) и `QuickCheck` (Property-based тесты).

#### 3.1 Примеры тестов

**Unit-тесты:**
```haskell
it "should count elements correctly" $ do
    let b = Bag.fromList ([1, 1, 2] :: [Int])
    Bag.count 1 b `shouldBe` 2
    Bag.count 2 b `shouldBe` 1

it "should remove elements correctly" $ do
    let b = Bag.fromList ([1, 1, 2] :: [Int])
    let b' = Bag.remove 1 b
    Bag.count 1 b' `shouldBe` 1
```

**Property-based тесты (Свойства моноида):**
```haskell
-- Ассоциативность: (a <> b) <> c == a <> (b <> c)
prop_monoid_assoc :: TestBag Int -> TestBag Int -> TestBag Int -> Bool
prop_monoid_assoc (TestBag b1) (TestBag b2) (TestBag b3) =
    (b1 <> b2) <> b3 == b1 <> (b2 <> b3)

-- Инвариант вставки: добавление элемента всегда увеличивает счетчик
prop_insert_delete :: Int -> TestBag Int -> Bool
prop_insert_delete x (TestBag bag) =
    let bag' = Bag.insert x bag
    in Bag.count x bag' == Bag.count x bag + 1
```

#### 3.2 Отчет о запуске тестов

```text
OABag Unit Tests
  should be empty initially
  should count elements correctly
  should remove elements correctly
  should handle filtering
  should map elements
  should merge duplicate keys on map
OABag Properties
  Monoid Left Identity
    +++ OK, passed 100 tests.
  Monoid Right Identity
    +++ OK, passed 100 tests.
  Monoid Associativity
    +++ OK, passed 100 tests.
  Insert increases count
    +++ OK, passed 100 tests.
  Remove decreases count
    +++ OK, passed 100 tests.
  Roundtrip list conversion preserves elements
    +++ OK, passed 100 tests.

Finished in 0.0502 seconds
12 examples, 0 failures
```

---

### 4. Выводы

В ходе работы была реализована структура данных Bag на основе хеш-таблицы с открытой адресацией в функциональном стиле.

#### Сравнение подходов (Haskell vs Python/Go)

1.  **Неизменяемость vs Производительность:**
    *   В **Python** (`dict`) или **Go** (`map`) хеш-таблицы являются изменяемыми структурами. Вставка элемента имеет амортизированную сложность $O(1)$.
    *   В **Haskell** требование неизменяемости (Persistent Data Structure) в сочетании с алгоритмом Open Addressing (который требует плоского массива) приводит к необходимости полного копирования вектора при каждой модификации ($O(N)$). В реальных проектах на Haskell для таких задач используют деревья (HAMT) или `ST`-монаду для локальной мутации, но в рамках учебной задачи это демонстрирует цену абстракции "чистоты".

2.  **Типизация и надежность:**
    *   **Python:** Динамическая типизация позволяет легко писать код, но ошибки типов (например, попытка сложить несовместимые элементы в Bag) выявляются только в рантайме.
    *   **Go:** Статическая типизация, но отсутствие дженериков (до недавнего времени) или их ограниченность заставляет писать больше шаблонного кода.
    *   **Haskell:** Система типов (Typeclasses `Hashable`, `Eq`, `Monoid`) гарантирует корректность операций на этапе компиляции. Property-based тестирование (`QuickCheck`) позволяет проверить фундаментальные законы (например, ассоциативность объединения Bag), что гораздо сложнее реализовать в Go или Python.

3.  **Выразительность:**
    *   Реализация методов `map`, `filter`, `fold` в Haskell занимает 1-2 строки благодаря функциям высшего порядка и композиции, тогда как в Go это потребовало бы написания явных циклов `for`.

**Итог:** Реализация Open Addressing на неизменяемых структурах — алгоритмически неэффективное, но полезное упражнение для понимания работы памяти и системы типов в функциональных языках. Использование линтера и CI гарантирует высокий стандарт качества кода.