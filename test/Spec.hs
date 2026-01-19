{-# LANGUAGE ScopedTypeVariables #-}
-- Игнорируем правила HLint т.к тестируем свойства моноида
{- HLINT ignore "Monoid law, left identity" -}
{- HLINT ignore "Monoid law, right identity" -}

import Test.Hspec
import Test.QuickCheck
import qualified OABag as Bag
import Data.List (sort)
import Data.Hashable (Hashable)

newtype TestBag a = TestBag { unBag :: Bag.OABag a }
    deriving (Show, Eq)

instance (Hashable a, Eq a, Arbitrary a) => Arbitrary (TestBag a) where
    arbitrary = TestBag . Bag.fromList <$> arbitrary

-- Свойства моноида
prop_monoid_identity_left :: TestBag Int -> Bool
prop_monoid_identity_left (TestBag bag) = 
    mempty <> bag == bag

prop_monoid_identity_right :: TestBag Int -> Bool
prop_monoid_identity_right (TestBag bag) = 
    bag <> mempty == bag

prop_monoid_assoc :: TestBag Int -> TestBag Int -> TestBag Int -> Bool
prop_monoid_assoc (TestBag b1) (TestBag b2) (TestBag b3) =
    (b1 <> b2) <> b3 == b1 <> (b2 <> b3)

-- Свойства Bag
prop_insert_delete :: Int -> TestBag Int -> Bool
prop_insert_delete x (TestBag bag) =
    let bag' = Bag.insert x bag
    in Bag.count x bag' == Bag.count x bag + 1

prop_delete_decrements :: Int -> [Int] -> Property
prop_delete_decrements x xs = 
    let bag = Bag.insert x (Bag.fromList xs)
    in property $ Bag.count x (Bag.remove x bag) == Bag.count x bag - 1

prop_fromList_toList_sort :: [Int] -> Bool
prop_fromList_toList_sort xs =
    sort (Bag.toList (Bag.fromList xs)) == sort xs

main :: IO ()
main = hspec $ do
    describe "OABag Unit Tests" $ do
        it "should be empty initially" $ do
            Bag.size Bag.empty `shouldBe` 0
        
        it "should count elements correctly" $ do
            let b = Bag.fromList ["a", "b", "a"]
            Bag.count "a" b `shouldBe` 2
            Bag.count "b" b `shouldBe` 1
            Bag.count "c" b `shouldBe` 0
            
        it "should remove elements correctly" $ do
            let b = Bag.fromList ([1, 1, 2] :: [Int])
            let b' = Bag.remove 1 b
            Bag.count 1 b' `shouldBe` 1
            Bag.size b' `shouldBe` 2
            
            let b'' = Bag.remove 1 b'
            Bag.count 1 b'' `shouldBe` 0
            Bag.size b'' `shouldBe` 1

        it "should handle filtering" $ do
            let b = Bag.fromList ([1, 2, 3, 4] :: [Int])
            let b' = Bag.filterBag even b
            Bag.toList b' `shouldMatchList` [2, 4]

        it "should map elements" $ do
            let b = Bag.fromList ([1, 2, 3] :: [Int]) 
            let b' = Bag.mapBag (*2) b
            Bag.toList b' `shouldMatchList` [2, 4, 6]
            
        it "should merge duplicate keys on map" $ do
            let b = Bag.fromList ([1, 2] :: [Int]) 
            let b' = Bag.mapBag (const (0 :: Int)) b
            Bag.count 0 b' `shouldBe` 2

    describe "OABag Properties" $ do
        it "Monoid Left Identity" $ property prop_monoid_identity_left
        it "Monoid Right Identity" $ property prop_monoid_identity_right
        it "Monoid Associativity" $ property prop_monoid_assoc
        it "Insert increases count" $ property prop_insert_delete
        it "Remove decreases count" $ property (prop_delete_decrements :: Int -> [Int] -> Property)
        it "Roundtrip list conversion preserves elements" $ property (prop_fromList_toList_sort :: [Int] -> Bool)