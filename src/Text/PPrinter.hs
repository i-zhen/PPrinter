-- This library is also available at https://hackage.haskell.org/package/PPrinter

{-# LANGUAGE DeriveGeneric, TypeOperators, FlexibleInstances, FlexibleContexts, DefaultSignatures #-}

module Text.PPrinter (
  Pretty(..),
  Style(..),

  -- Instances for Pretty: (), Bool, Ordering, Int, Integer, Char, String, Float, Double

  -- Pretty support code
  pprint, pprintLen, pprintStyle,
  Generic
  ) where

import Data.Map hiding (showTree, map, null, foldr)
import GHC.Generics
import Data.List (null)
import Data.Char

infixr 5     :<|>
infixr 6     :<>
infixr 6     <>
infixr 6     <+>
infixr 6     <->

data DOC     = NIL
             | DOC :<> DOC
             | NEST Int DOC
             | TEXT String
             | LINE
             | DOC :<|> DOC
               deriving(Show)

data Doc     = Nil
             | String `Text` Doc
             | Int `Line` Doc
               deriving(Show)

-- interface

nil          = NIL
x <> y       = x :<> y
x <+> y      = x <> whiteSpace <> y
nest         = NEST
text         = TEXT
line         = LINE

lpar         = text "("
rpar         = text ")"
comma        = text ","
whiteSpace   = text " "
parens s     = lpar <> s <> rpar

group x      = flatten x :<|> x

indent       = 1

-- implementation

flatten NIL          = NIL
flatten (x :<> y)    = flatten x :<> flatten y
flatten (NEST i x)   = NEST i (flatten x)
flatten (TEXT s)     = TEXT s
flatten LINE         = TEXT " "
flatten (x :<|> y)   = flatten x


layout Nil           = ""
layout (s `Text` x)  = s ++ layout x
layout (i `Line` x)  = '\n' : copy i ' ' ++ layout x

-- interfaces for oneLineMode
oneLayout Nil           = ""
oneLayout (s `Text` x)  = s ++ oneLayout x
oneLayout (i `Line` x)  = ' ' : oneLayout x

copy i x             = [ x | _ <- [1 .. i] ]

best w k x           = be w k [(0, x)]

be w k []               = Nil
be w k ((i,NIL):z)      = be w k z
be w k ((i,x :<> y):z)  = be w k ((i,x):(i,y):z)
be w k ((i,NEST j x):z) = be w k ((i+j,x):z)
be w k ((i,TEXT s):z)   = s `Text` be w (k+length s) z
be w k ((i,LINE):z)     = i `Line` be w i z
be w k ((i,x :<|> y):z) = better w k (be w k ((i,x):z))
                                    (be w k ((i,y):z))

better w k x y         = if fits (w-k) x then x else y

fits w x | w < 0       = False
fits w Nil             = True
fits w (s `Text` x)    = fits (w - length s) x
fits w (i `Line` x)    = True


-- class GPretty

data Type = Infixt String | Prefixt | Recordt

class GPretty f where

  -- 'gpp' is the (*->*) kind equivalent of 'pp'
  gpp    :: Type     -- The type of fixity. Record, Infix or Prefix.
            -> Int   -- The operator precedence
            -> Bool  -- Flag that marks if the constructors was wrapped in parens
            -> f a
            -> [DOC] -- The result.

  -- 'nullary' marks nullary constructors
  nullary :: f x -> Bool

instance GPretty U1 where
  gpp _ _ _ _ = []
  nullary _   = True

-- ignore tagging
-- K1 : Constants, additional parameters and recursion of kind *
instance (Pretty a) => GPretty (K1 i a) where
  gpp _ n _ (K1 x) = [ppPrec n x]
  nullary _        = False

instance (GPretty a, GPretty b) => GPretty (a :+: b) where
  gpp t d b (L1 x) = gpp t d b x
  gpp t d b (R1 x) = gpp t d b x
  nullary (L1 x) = nullary x
  nullary (R1 x) = nullary x

instance (GPretty a, GPretty b) => GPretty (a :*: b) where
  gpp t1@Recordt d flag (a :*: b) = gppa ++ [comma, line] ++ gppb
    where
      gppa = gpp t1 d flag a
      gppb = gpp t1 d flag b

  gpp t1@Prefixt d flag (a :*: b) = gppa ++ [line] ++ gppb
    where
      gppa = gpp t1 d flag a
      gppb = gpp t1 d flag b

  gpp t1@(Infixt s) d flag (a :*: b) = init gppa ++ [last gppa <+> text s] ++ addWhitespace gppb
    where
      gppa = gpp t1 d flag a
      gppb = gpp t1 d flag b

      -- add whitespace
      addWhitespace :: [DOC] -> [DOC]
      addWhitespace [] = []
      addWhitespace m@(x:xs)
        | paren == 0 = if flag then map (nest 1) (line : m) else line : m
        | otherwise = map (nest $ white + 1 + (if flag then 1 else 0)) (line :  m)
        where
          sa = Prelude.filter (\x -> x /= '\n') $ pretty layout 1 x
          sb = Prelude.filter (\x -> x /= '\n') $ pretty layout 1 (head gppa)
          paren = length $ takeWhile (== '(') sa
          white = length $ takeWhile( /= ' ') (dropWhile(== '(') sb)

  nullary _ = False

-- ignore datatype meta-information
-- data D : Tag for M1: datatype
instance (GPretty a, Datatype c) => GPretty (M1 D c a) where
  gpp t d b (M1 x) = gpp t d b x
  nullary (M1 x)   = nullary x

-- selector, display the name of it
-- data S : Tag for M1: record selector
instance (GPretty f, Selector c) => GPretty (M1 S c f) where
  gpp t d b s@(M1 a)
                | null selector = gpp t d b a
                | otherwise = (text selector <+>  char '=' <> whiteSpace) : map (nest $ length selector + 2) (gpp t 0 b a)
      where
          selector = selName s

  nullary (M1 x) = nullary x

-- constructor, show prefix operators
-- data C : Tag for M1: constructor
instance (GPretty f, Constructor c) => GPretty (M1 C c f) where
  gpp _ d b c@(M1 a) =
    case conFixity c of
      Prefix -> wrapParens checkIfWrap $
        text (conName c) <> whiteSpace
        : addWhitespace checkIfWrap (wrapRecord (gpp t 11 b a))
      Infix _ l ->
        wrapParens (d > l) $ gpp t (l + 1) (d > l) a
      where
        t = if conIsRecord c then Recordt else
            case conFixity c of
              Prefix    -> Prefixt
              Infix _ _ -> Infixt (conName c)

        checkIfWrap = not (nullary a) && (d > 10)

        -- add whitespace
        addWhitespace :: Bool     -- check if wrap parens
                         -> [DOC]
                         -> [DOC]
        addWhitespace _ [] = []
        addWhitespace b s | conIsRecord c = s
                          | otherwise = map (nest $ length (conName c) + if b then 2 else 1) s

        -- add braces for record
        wrapRecord :: [DOC] -> [DOC]
        wrapRecord [] = []
        wrapRecord s | conIsRecord c = wrapNest s
                     | otherwise = s
                     where
                       wrapNest2       = foldr (\x -> (++) [nest (length (conName c) + 2) x]) [text "}"]
                       wrapNest (x:xs) = nest (length (conName c) + 1) (text "{" <> x) : wrapNest2 xs

        -- add Parens
        wrapParens :: Bool       -- add parens or not
                      -> [DOC]
                      -> [DOC]
        wrapParens _ [] = []
        wrapParens False s = s
        wrapParens True (x:xs) = lpar <> x : wrapParens2 xs
                   where
                     wrapParens2 = foldr (:) [rpar]

  nullary (M1 x) = nullary x


class Pretty a where

  -- | 'ppPrec' converts a value to a pretty printable DOC.
  --
  ppPrec :: Int   -- ^ the operator precedence of the enclosing context
           -> a   -- ^ the value to be converted to a 'String'
           -> DOC -- ^ the result
  default ppPrec :: (Generic a, GPretty (Rep a)) => Int -> a -> DOC
  ppPrec n x = rep $ gpp Prefixt n False (from x)

  -- | 'pp' is the equivalent of 'Prelude.show'
  --
  pp     :: a -> DOC
  default pp :: (Generic a, GPretty (Rep a)) => a -> DOC
  pp x = rep $ gpp Prefixt 0 False (from x)

  -- helper function for generating a DOC list
  genList :: [a] -> DOC
  genList [] = nil
  genList (x:xs) = comma <>
                   line <> whiteSpace <>
                   nest indent (pp x) <>
                   genList xs

  -- | 'ppList' is the equivalent of 'Prelude.showList'
  --
  ppList :: [a] -> DOC
  ppList []     = text "[]"
  ppList (x:xs) = group $
                  text "[" <>
                  nest indent (pp x) <> genList xs <>
                  text "]"
  {-# MINIMAL ppPrec | pp #-}


instance Pretty () where
  pp () = text "()"
  ppPrec _ = pp

instance Pretty Bool where
  pp b = text $ show b
  ppPrec _ = pp

instance Pretty Ordering where
  pp o = text $ show o
  ppPrec _ = pp

instance Pretty Int where
  ppPrec n x
    | n /= 0 && x < 0 = parens (text $ show x)
    | otherwise = text $ show x
  pp = ppPrec 0

instance Pretty Integer where
  ppPrec n x
    | n /= 0 && x < 0 = parens (text $ show x)
    | otherwise = text $ show x
  pp = ppPrec 0

instance Pretty Float where
  ppPrec n x
    | n /= 0 && x < 0 = parens (text $ show x)
    | otherwise = text $ show x
  pp = ppPrec 0

instance Pretty Double where
  ppPrec n x
    | n /= 0 && x < 0 = parens (text $ show x)
    | otherwise = text $ show x
  pp = ppPrec 0

instance Pretty Char where
  pp char = text $ show char
  ppPrec _ = pp
  -- instance Pretty String where , as below
  ppList str = text $ show str

-- doc ([1,3,7] :: [Int])
instance Pretty a => Pretty [a] where
  pp = ppList
  ppPrec _ = pp

instance (Pretty a, Pretty b) => Pretty (Map a b) where
  pp m =  group $ "fromList" <-> pp (toList m)
  ppPrec _ = pp

instance Pretty a => Pretty (Maybe a) where
  ppPrec n Nothing = text "Nothing"
  ppPrec n (Just x)
    | n /= 0 = parens s
    | otherwise = s
      where
        s = "Just" <-> ppPrec 10 x
  pp = ppPrec 0

instance (Pretty a, Pretty b) => Pretty (Either a b) where
  ppPrec n (Left x)
    | n /= 0 = parens s
    | otherwise = s
      where
        s = "Left" <-> ppPrec 10 x
  ppPrec n (Right x)
    | n /= 0 = parens s
    | otherwise = s
      where
        s = "Right" <-> ppPrec 10 x
  pp = ppPrec 0

-- instances for the first few tuples

instance (Pretty a, Pretty b) => Pretty (a, b) where
  pp (a, b) =  group (parens $ sep [pp a <> comma, pp b])
  ppPrec _ = pp

instance (Pretty a, Pretty b, Pretty c) => Pretty (a, b, c) where
  pp (a, b, c) =  group (parens $ sep [pp a <> comma, pp b <> comma, pp c])
  ppPrec _ = pp

instance (Pretty a, Pretty b, Pretty c, Pretty d) => Pretty (a, b, c, d) where
  pp (a, b, c, d) =  group (parens $ sep [pp a <> comma, pp b <> comma, pp c <> comma, pp d])
  ppPrec _ = pp

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e) => Pretty (a, b, c, d, e) where
  pp (a, b, c, d, e) =  group (parens $ sep [pp a <> comma, pp b <> comma,
                                             pp c <> comma, pp d <> comma, pp e])
  ppPrec _ = pp

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e, Pretty f) => Pretty (a, b, c, d, e, f) where
  pp (a, b, c, d, e, f) =  group (parens $ sep [pp a <> comma, pp b <> comma,
                                                pp c <> comma, pp d <> comma,
                                                pp e <> comma, pp f])
  ppPrec _ = pp

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e, Pretty f, Pretty g)
          => Pretty (a, b, c, d, e, f, g) where
  pp (a, b, c, d, e, f, g) =  group (parens $ sep [pp a <> comma, pp b <> comma, pp c <> comma,
                                                   pp d <> comma, pp e <> comma, pp f <> comma,
                                                   pp g])
  ppPrec _ = pp

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e, Pretty f, Pretty g, Pretty h)
          => Pretty (a, b, c, d, e, f, g, h) where
  pp (a, b, c, d, e, f, g, h) =  group (parens $ sep [pp a <> comma, pp b <> comma, pp c <> comma,
                                                      pp d <> comma, pp e <> comma, pp f <> comma,
                                                      pp g <> comma, pp h])
  ppPrec _ = pp

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e, Pretty f, Pretty g, Pretty h, Pretty i)
          => Pretty (a, b, c, d, e, f, g, h, i) where
  pp (a, b, c, d, e, f, g, h, i) =  group (parens $ sep [pp a <> comma, pp b <> comma, pp c <> comma,
                                                         pp d <> comma, pp e <> comma, pp f <> comma,
                                                         pp g <> comma, pp h <> comma, pp i])
  ppPrec _ = pp

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e, Pretty f, Pretty g, Pretty h, Pretty i,
          Pretty j)
          => Pretty (a, b, c, d, e, f, g, h, i, j) where
  pp (a, b, c, d, e, f, g, h, i, j)
      =  group (parens $ sep [pp a <> comma, pp b <> comma, pp c <> comma, pp d <> comma,
                              pp e <> comma, pp f <> comma, pp g <> comma, pp h <> comma,
                              pp i <> comma, pp j])
  ppPrec _ = pp

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e, Pretty f, Pretty g, Pretty h, Pretty i,
          Pretty j, Pretty k)
          => Pretty (a, b, c, d, e, f, g, h, i, j, k) where
  pp (a, b, c, d, e, f, g, h, i, j, k)
      =  group (parens $ sep [pp a <> comma, pp b <> comma, pp c <> comma, pp d <> comma,
                              pp e <> comma, pp f <> comma, pp g <> comma, pp h <> comma,
                              pp i <> comma, pp j <> comma, pp k])
  ppPrec _ = pp

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e, Pretty f, Pretty g, Pretty h, Pretty i,
          Pretty j, Pretty k, Pretty l)
          => Pretty (a, b, c, d, e, f, g, h, i, j, k, l) where
  pp (a, b, c, d, e, f, g, h, i, j, k, l)
      =  group (parens $ sep [pp a <> comma, pp b <> comma, pp c <> comma, pp d <> comma,
                              pp e <> comma, pp f <> comma, pp g <> comma, pp h <> comma,
                              pp i <> comma, pp j <> comma, pp k <> comma, pp l])
  ppPrec _ = pp

-------------------------------------------------------------
-- Support code for Pretty
-------------------------------------------------------------

-- helper function that get the value from char type to DOC
char :: Char -> DOC
char chr = text [chr]

-- helper functions for instance Pretty Pair and List
-- generate n spaces
text' :: Int -> String
text' n | n == 0 = ""
        | otherwise = " " ++ text' (n - 1)

-- helper function for docList
pp' :: Pretty a => a -> DOC
pp' x = nest indent (line <> pp x)

-- helper function for reproducing the [DOC] to DOC
rep :: [DOC] -> DOC
rep []     = nil
rep (x:xs) = group $ Prelude.foldl (<>) nil (x:xs)

sep :: [DOC] -> DOC
sep []     = nil
sep (x:xs) = nest indent x
             <> foldr1 (\l r -> l <> nil <> r) (map (\x -> nest indent (line <> x)) xs)

x <-> y = text x <+> nest (length x + 1) y

pretty :: (Doc -> String) -> Int -> DOC -> String
pretty f w x  = f (best w 0 x)

pshow :: Pretty a => (Doc -> String) -> Int -> a -> String
pshow f w x = pretty f w (pp x <> line)

pprinter :: Pretty a => Int -> a -> IO()
pprinter w x = putStr (pshow layout w x)

-------------------------------------------------------------
-- Pretty Printer
-------------------------------------------------------------

data Mode = ManyLineMode | OneLineMode

-- | A rendering style
data Style = Style { mode    :: Mode,  -- ^ The redering mode
                     lineLen :: Int    -- ^ Length of line
                   }

styleMode :: Style -> Mode
styleMode (Style mode length) = mode

styleLen  :: Style -> Int
styleLen (Style mode length) = length

-- | The default 'Style'
style :: Style
style = Style {mode = ManyLineMode, lineLen = 40}

render     :: Show a => Pretty a => a -> String
fullRender :: Show a => Pretty a =>
              Mode
              -> Int
              -> a
              -> String
fullRender ManyLineMode w x = pshow layout w x
fullRender OneLineMode w x = pshow oneLayout w x

-- use default style
render = fullRender (styleMode style) (styleLen style)

pprint :: Show a => Pretty a => a -> IO()
pprint x = putStr (render x)

pprintLen :: Show a => Pretty a => Int -> a -> IO()
pprintLen = pprinter

-- | The default Pretty Printer
pprintStyle :: Show a => Pretty a => Style -> a -> IO()
pprintStyle s x = putStr $ fullRender (styleMode s) (styleLen s) x
