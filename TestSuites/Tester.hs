{-# LANGUAGE DeriveGeneric #-}

module Tester where

import Text.PPrinter hiding (char, (<|>))
import Test.Hspec
import Test.QuickCheck

import Data.Map hiding (showTree, map, null)
import Data.List (null)
import Data.Char
import Control.Exception (evaluate)

import Control.Applicative hiding (many, (<|>))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

-------------------------------------------------------------
-- Parser
-------------------------------------------------------------

-- separator
sepParser :: Parser ()
sepParser = spaces >> char ',' >> spaces


-- literal string
stringParser :: Parser String
stringParser = do
             char '\"'
             manyTill (letter <|> digit <|> space) (char '\"') <|> string "\\\""


numParser :: Parser String
numParser = many (char '-' <|> digit <|> char '.' <|> char 'e')

strParser :: Parser String
strParser = stringParser <|> string "True" <|> string "False" <|> numParser

-- (String, Int)
pairParser :: Parser (String, String)
pairParser = do
           char '('
           a <- stringParser
           spaces
           char ','
           spaces
           b <- many1 digit
           char ')'
           return (a, b)

-- list of pair
listParser :: Parser [(String, String)]
listParser = sepBy pairParser sepParser

listParser2' :: Parser [String]
listParser2' = sepBy strParser sepParser

listParser2 :: Parser [String]
listParser2 = do
            char '['
            e <- listParser2'
            char ']'
            return e

mapParser :: Parser [(String, String)]
mapParser = do
          string "fromList"
          spaces
          char '['
          e <- listParser
          char ']'
          return e

tree1Parser :: Parser String
tree1Parser = (string "Nod" >>
              spaces >> char '(' >> tree1Parser >>= \res1 -> char ')' >>
              spaces >> char '(' >> tree1Parser >>= \res2 -> char ')' >>
              spaces >> return (res1 ++ res2)) <|>
             (string "Leaf" >> spaces >> strParser)

tree2Parser :: Parser String
tree2Parser =(char '(' >> tree2Parser >>= \res1 -> char ')' >> spaces >> string ":^:" >>
              spaces >> char '(' >> tree2Parser >>= \res2 -> char ')' >>
              return (res1 ++ res2)) <|>
             (string "Leaf2" >> spaces >> strParser)

infixParser :: Parser String
infixParser = numParser >>= \res1 -> spaces >> string ":**:" >> spaces >>
              strParser >>= \res2 -> return (res1 ++ res2)


recParser :: Parser String
recParser = string "Person" >> spaces >>
            string "{firstName = " >> strParser >>= \res1 -> char ',' >> spaces >>
            string "lastName = " >> strParser >>= \res2 -> char ',' >> spaces >>
            string "age = " >> strParser >>= \res3 -> char ',' >> spaces >>
            string "height = " >> strParser >>= \res4 -> char ',' >> spaces >>
            string "addr = " >> strParser >>= \res5 -> char ',' >> spaces >>
            string "occup = " >> strParser >>= \res6 -> char ',' >> spaces >>
            string "gender = " >> strParser >>= \res7 -> char ',' >> spaces >>
            string "nationality = " >> strParser >>= \res8 ->
            return (res1 ++ res2 ++ res3 ++ res4 ++ res5 ++ res6 ++ res7 ++ res8)


testRec = parse recParser ""

testInfix = parse infixParser ""

-- the interface of testing Trees
testTree1 = parse tree1Parser ""

testTree2 = parse tree2Parser ""

-- the interface of testing Map
testMap = parse mapParser ""

testList = parse listParser ""

-------------------------------------------------------------
-- Utility functions
-------------------------------------------------------------

-- omit the white space and new line
-- For example, omitNewline "a b c \n" will return "abc"
omitNewline :: String -> String
omitNewline [] = []
omitNewline (x:xs) = Prelude.filter (/= '\n') (x:xs)

omitWhite :: String -> String
omitWhite [] = []
omitWhite (x:xs) = Prelude.filter (\x -> x /= ' ' && x /= '\n') (x:xs)

pShow :: Pretty a => Int -> a -> String
pShow w x = pretty layout w (pp x <> line)

-------------------------------------------------------------
-- Main function
-------------------------------------------------------------

main :: IO ()
main = hspec $ do

-------------------------------------------------------------
-- Primitive data types
-------------------------------------------------------------

  describe "Primitive Data Type Testing" $ do

    -- Unit

    it "Unit" $ property $
      \x -> omitNewline (pShow x ()) `shouldBe` show ()

    -- Number test

    it "Num : test positive integer" $
      omitNewline (pShow 10 (10 ::Int)) `shouldBe` show 10

    it "Num : test negative integer" $
      omitNewline (pShow 10 ((-999) ::Int)) `shouldBe` show (-999)

    it "Num : test big integer" $
      omitNewline (pShow 10 (999999999999999999999999 ::Integer)) `shouldBe`
                  show 999999999999999999999999

    it "Num : random integer test" $ property $
       \x y -> omitNewline (pShow y (x :: Int)) `shouldBe` show x

    it "Num : random float test" $ property $
       \x y -> omitNewline (pShow y (x :: Float)) `shouldBe` show x

    it "Num : random double test" $ property $
       \x y -> omitNewline (pShow y (x :: Double)) `shouldBe` show x

    -- Char and String test

    it "Char : random character" $ property $
       \x y -> init (pShow y (x :: Char)) `shouldBe` show x

    it "String : random string" $ property $
       \x y -> init (pShow y (x :: String)) `shouldBe` show x

    -- Bool test

    it "Bool : random boolean" $ property $
       \x y -> omitNewline (pShow y (x :: Bool)) `shouldBe` show x

    -- Map test

    it "Map  : test 1" $ property $
       \x -> testMap (pShow x m1) `shouldBe` testMap (show m1)

    it "Map  : test 2" $ property $
       \x -> testMap (pShow x m1) `shouldBe` testMap (show m1)

    it "Map  : test 3" $ property $
       \x -> testMap (pShow x m1) `shouldBe` testMap (show m1)

    -- Ordering
    it "Ordering : EQ | LT | GT" $ property $
       \x y -> omitNewline (pShow x (y :: Ordering)) `shouldBe` show y

    -- Maybe
    it "Maybe : Nothing" $ property $
       \x -> omitNewline (pShow x (Nothing :: Maybe Int)) `shouldBe`
             show (Nothing :: Maybe Int)

    it "Maybe : Maybe Bool" $ property $
       \x -> omitNewline (pShow x (Just True :: Maybe Bool)) `shouldBe`
             omitNewline (show (Just True :: Maybe Bool))

    it "Maybe : Maybe Int" $ property $
       \x y -> omitNewline (pShow x (Just y :: Maybe Int)) `shouldBe`
               omitNewline (show (Just y :: Maybe Int))

    it "Maybe : Maybe Char" $ property $
       \x y -> omitNewline (pShow x (Just y :: Maybe Char)) `shouldBe`
               omitNewline (show (Just y :: Maybe Char))

    it "Maybe : Maybe String" $ property $
       \x y -> omitNewline (pShow x (Just y :: Maybe String)) `shouldBe`
               omitNewline (show (Just y :: Maybe String))

    it "Maybe : Maybe Float" $ property $
       \x y -> omitNewline (pShow x (Just y :: Maybe Float)) `shouldBe`
               omitNewline (show (Just y :: Maybe Float))

    it "Maybe : Maybe Double" $ property $
       \x y -> omitNewline (pShow x (Just y :: Maybe Double)) `shouldBe`
               omitNewline (show (Just y :: Maybe Double))

    it "Maybe : Maybe Ordering" $ property $
       \x y -> omitNewline (pShow x (Just y :: Maybe Ordering)) `shouldBe`
               omitNewline (show (Just y :: Maybe Ordering))

    -- Either

    it "Either : Left Int" $ property $
       \x y -> omitNewline (pShow x (Left y :: Either Int Int)) `shouldBe`
               omitNewline (show (Left y :: Either Int Int))

    it "Either : Right Integer" $ property $
       \x y -> omitNewline (pShow x (Right y :: Either Int Integer)) `shouldBe`
               omitNewline (show (Right y :: Either Int Integer))

    it "Either : Left (Maybe Int)" $ property $
       \x y -> omitNewline (pShow x (Left y :: Either (Maybe Int) Int)) `shouldBe`
               omitNewline (show (Left y :: Either (Maybe Int) Int))

    it "Either : Right Bool" $ property $
       \x y -> omitNewline (pShow x (Right y :: Either Int Bool)) `shouldBe`
               omitNewline (show (Right y :: Either Int Bool))

    it "Either : Left Char" $ property $
       \x y -> omitNewline (pShow x (Left y :: Either Char Int)) `shouldBe`
               omitNewline (show (Left y :: Either Char Int))

    it "Either : Right String" $ property $
       \x y -> omitNewline (pShow x (Right y :: Either Int String)) `shouldBe`
               omitNewline (show (Right y :: Either Int String))

    it "Either : Left Float" $ property $
       \x y -> omitNewline (pShow x (Left y :: Either Float Int)) `shouldBe`
               omitNewline (show (Left y :: Either Float Int))

    it "Either : Right Double" $ property $
       \x y -> omitNewline (pShow x (Right y :: Either Int Double)) `shouldBe`
               omitNewline (show (Right y :: Either Int Double))

    -- Pair

    it "Pair : (a, b)" $ property $
       \x y -> omitWhite (pShow x (y :: (Int, Int))) `shouldBe`
                omitWhite (show (y :: (Int, Int)))

    it "Triple : (a, b, c)" $ property $
       \x y -> omitWhite (pShow x (y :: (Int, Int, Bool))) `shouldBe`
                omitWhite (show (y :: (Int, Int, Bool)))

    it "Tuple : (a, b, c, d)" $ property $
       \x y -> omitWhite (pShow x (y :: (Int, Int, Bool, Float))) `shouldBe`
                omitWhite (show (y :: (Int, Int, Bool, Float)))

    it "Tuple : (a, b, c, d, e)" $ property $
       \x y -> omitWhite (pShow x (y :: (Int, Int, Bool, Double, Char))) `shouldBe`
                omitWhite (show (y :: (Int, Int, Bool, Double, Char)))

    it "Tuple : (a, b, c, d, e, f)" $ property $
       \x   -> omitWhite (pShow x ((True, False, True, False, True, False)
                                     :: (Bool, Bool, Bool,Bool, Bool, Bool)))
               `shouldBe`
                omitWhite (show  ((True, False, True, False, True, False)
                                     :: (Bool, Bool, Bool,Bool, Bool, Bool)))

    it "Tuple : (a, b, c, d, e, f, g)" $ property $
       \x   -> omitWhite (pShow x ((True, False, True, False, True, False, True, False)
                                     :: (Bool, Bool, Bool,Bool, Bool, Bool,Bool, Bool)))
               `shouldBe`
                omitWhite (show  ((True, False, True, False, True, False, True, False)
                                     :: (Bool, Bool, Bool,Bool, Bool, Bool,Bool, Bool)))

    it "Tuple : (a, b, c, d, e, f, g, h)" $ property $
       \x   -> omitWhite (pShow x ((True, False, True, False, True, False, True, False)
                                     :: (Bool, Bool, Bool,Bool, Bool, Bool,Bool, Bool)))
               `shouldBe`
                omitWhite (show  ((True, False, True, False, True, False, True, False)
                                     :: (Bool, Bool, Bool,Bool, Bool, Bool,Bool, Bool)))

    it "Tuple : (a, b, c, d, e, f, g, h, i)" $ property $
       \x   -> omitWhite (pShow x ((True, False, True, False, True, False, True, False, True)
                                     :: (Bool, Bool, Bool,Bool, Bool, Bool,Bool, Bool, Bool)))
               `shouldBe`
                omitWhite (show  ((True, False, True, False, True, False, True, False, True)
                                     :: (Bool, Bool, Bool,Bool, Bool, Bool,Bool, Bool, Bool)))

    it "Tuple : (a, b, c, d, e, f, g, h, i, j)" $ property $
       \x   -> omitWhite (pShow x ((True, False, True, False, True, False, True, False, True, False)
                                     :: (Bool, Bool, Bool,Bool, Bool, Bool,Bool, Bool, Bool,Bool)))
               `shouldBe`
                omitWhite (show  ((True, False, True, False, True, False, True, False, True, False)
                                     :: (Bool, Bool, Bool,Bool, Bool, Bool,Bool, Bool, Bool,Bool)))

    it "Tuple : (a, b, c, d, e, f, g, h, i, j, k)" $ property $
       \x   -> omitWhite (pShow x ((True, False, True, False, True, False, True, False, True, False, True)
                                     :: (Bool, Bool, Bool,Bool, Bool, Bool,Bool, Bool, Bool,Bool, Bool)))
               `shouldBe`
                omitWhite (show  ((True, False, True, False, True, False, True, False, True, False, True)
                                     :: (Bool, Bool, Bool,Bool, Bool, Bool,Bool, Bool, Bool,Bool, Bool)))

    it "Tuple : (a, b, c, d, e, f, g, h, i, j, k, l)" $ property $
       \x   -> omitWhite (pShow x ((True, False, True, False, True, False, True, False, True, False, True, False)
                                     :: (Bool, Bool, Bool,Bool, Bool, Bool,Bool, Bool, Bool,Bool, Bool, Bool)))
               `shouldBe`
                omitWhite (show  ((True, False, True, False, True, False, True, False, True, False, True, False)
                                     :: (Bool, Bool, Bool,Bool, Bool, Bool,Bool, Bool, Bool,Bool, Bool, Bool)))


    -- List
    it "List : [Int]" $ property $
       \x y -> testList (pShow x (y :: [Int])) `shouldBe` testList (show y)

    it "List : [Float]" $ property $
       \x y -> testList (pShow x ((0 : y) :: [Float])) `shouldBe` testList (show (0 : y))

    it "List : [Double]" $ property $
       \x y -> testList (pShow x ((1.0 : y) :: [Double])) `shouldBe` testList (show (1.0 : y))

    it "List : String" $ property $
       \x y -> testList (pShow x ((' ' : y) :: String)) `shouldBe` testList (show (' ' : y))

    it "List : [String]" $ property $
       \x y -> testList (pShow x ((" " : y) :: [String])) `shouldBe` testList (show (" " : y))

    it "List : [Bool]" $ property $
       \x y -> testList (pShow x ((True : y) :: [Bool])) `shouldBe` testList (show (True : y))

{- It does not work

    it "List : Infinite [Int]" $ property $
       \x y -> (omitNewline $ pShow x ([y ..] :: [Int])) `shouldBe` show [y ..]

-}

-------------------------------------------------------------
-- Algebraic data type
-------------------------------------------------------------

  describe "\nAlgebraic Data Type Testing" $ do

    -- Record

    it "Record : test 1" $ property $
       \x -> testRec (pShow x pers) `shouldBe`
             testRec (show pers)

    -- Tree

    it "Trees Int" $ property $
       \x -> testTree1 (pShow x tree1) `shouldBe`
             testTree1 (show tree1)

    -- Infix notation

    it "Infix style : data Foo a b = a :**: b " $ property $
       \x -> testInfix (pShow x test1) `shouldBe`
             testInfix (show test1)


-------------------------------------------------------------
-- Data types from Repository of Haskell Programs
-------------------------------------------------------------

    it "Tree example from GHC.Show" $ property $
       \x -> testTree2 (pShow x tree2) `shouldBe`
             testTree2 (show tree2)


-------------------------------------------------------------
-- Definition for algebraic data type testing
-------------------------------------------------------------

-- Maps

m1 :: Map String Int
m1 = fromList [("ad", 123), ("b", 234), ("c", 345), ("d", 45)]

m2 :: Map String Int
m2 = singleton "abc" 123

-- tree example

data Tree = Node String [Tree] deriving (Generic, Show)

instance Pretty (Tree)

tree                 = Node "aaa" [
                            Node "bbbbb" [
                               Node "ccc" [],
                               Node "dd" []
                               ],
                            Node "eee" [],
                            Node "ffff" [
                              Node "gg" [],
                              Node "hhh" [],
                              Node "ii" []
                              ]
                            ]

data Foo a b = a :**: b deriving (Generic, Show)

data Trees a = Leaf a | Nod (Trees a) (Trees a) deriving (Generic, Show)

data Person = Person { firstName :: String,
                       lastName :: String,
                       age :: Int,
                       height :: Float,
                       addr :: String,
                       occup :: String,
                       gender :: Bool,
                       nationality :: String
                     } deriving (Generic, Show)

instance (Pretty a, Pretty b) => Pretty (Foo a b)

instance (Pretty a) => Pretty (Trees a)

instance Pretty (Person)

pers = Person "Arthur" "Lee" 20 (-1.75) "Edinburgh UK"
       "Student" True "Japan"

test1 = (2 :: Float) :**: "cc"

tree1 :: Trees Int
tree1 = Nod (Nod (Leaf 333)
                 (Leaf 5555))
            (Nod (Nod(Nod(Leaf 8888)
                         (Leaf 5757))
                  (Leaf 14414))
             (Leaf 32777))

-- Example from GHC.Show
infixr 5 :^:
data Tree2 a =  Leaf2 a  |  Tree2 a :^: Tree2 a deriving (Generic, Show)

instance (Pretty a) => Pretty (Tree2 a)
tree2 :: Tree2 Int
tree2 = (Leaf2 89) :^: ((((Leaf2 1324324) :^:
                          (Leaf2 1341)) :^: (Leaf2 (-22))) :^: (Leaf2 99))

style2 = Style {mode = ManyLineMode, lineLen = 40}
