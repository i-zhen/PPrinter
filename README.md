### Find in Hackage as well

https://hackage.haskell.org/package/PPrinter

### How to install.

#### For *nix operating systems:

1. Go to the directory of ./src.
2. Using the command 'cabal configure' and then 'cabal install'

#### For windows or other operating systems:

Please check the official instructions for Haskell.

### How to use this library

1. Open a new Haskell file and adding the pragma 'DeriveGeneric'
2. Adding the line 'import Text.PPrinter'.
3. Define a new type by following the steps below:

i. Define a data type with deriving mechanism at first:

```
data Tree = Node String [Tree] deriving (Generic, Show)
```

You should add ‘deriving (Generic, Show)’ in the end, both of ‘Generic’ and ‘Show’ are necessary.

ii. Define an EMPTY Pretty instance of the data type:

```
instance Pretty (Tree)
```

iii. Define the exact data of the type:

```

tree = Node "aaa" [
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

```

iv. Output the data by the builtin functions:

```
$ pprint tree

$ Node "aaa"
       [Node "bbbbb"
             [Node "ccc" [],
              Node "dd" []],
        Node "eee" [],
        Node "ffff"
             [Node "gg" [],
              Node "hhh" [],
              Node "ii" []]]
```

