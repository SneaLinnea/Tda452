
import Test.QuickCheck
import Data.Char
import Data.List

-------------------------------------------------------------------------

-- | Representation of sudoku puzzlese (allows some junk)
data Sudoku = Sudoku { rows :: [[Maybe Int]] }
 deriving ( Show, Eq )

-- | A sample sudoku puzzle
example :: Sudoku
example =
    Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

-- * A1

-- | allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9 (replicate 9 Nothing))

-- * A2

-- | isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku (Sudoku sud) = length sud == 9 && and (map ((==) 9 . length) sud)
  && and [test x| xs <- sud, x <- xs]
  where
    test Nothing = True
    test (Just n) = n >= 1 && n <= 9

-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled (Sudoku sud) = not $ or $ map (elem Nothing) sud

-------------------------------------------------------------------------

-- * B1

-- |b printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printSudoku :: Sudoku -> IO ()
printSudoku (Sudoku sud) = putStr $ unlines
                                  $ map (concat . map maybeToString) sud
  where
    maybeToString Nothing  = "."
    maybeToString (Just n) = show n

-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku file = do
    text <- readFile file
    let sud = Sudoku $ map (map charToMaybe) $ lines text
    if isSudoku sud
    then return sud
    else error "Not a sudoku!"
    where
      charToMaybe '.' = Nothing
      charToMaybe c = Just (digitToInt c)


-------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = frequency [(9,return Nothing),
                   (1, do c <- choose (1,9)
                          return (Just c))]

-- * C2

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- vectorOf 9 (vectorOf 9 cell)
       return (Sudoku rows)

-- * C3

-- | Property: a generated Sudoku is in fact a sudoku
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku sud = isSudoku sud

-------------------------------------------------------------------------

-- | a Block is a column, row or 3*3 cell block
type Block = [Maybe Int]

-- * D1

-- | a block can not contain the same digit twice
isOkayBlock :: Block -> Bool
isOkayBlock [] = True
isOkayBlock (Nothing:xs) = isOkayBlock xs
isOkayBlock (x:xs) = not (elem x xs) && isOkayBlock xs

-- * D2

-- | given a sudoku, create a list of all blocks of that Sudoku
blocks :: Sudoku -> [Block]
blocks (Sudoku sud) = sud ++ transpose sud ++
  [squareBlocks sud (x,y) |x <- [0..2], y <-[0..2]]
    where squareBlocks sud (x,y) = concat $ map (take 3) $ map (drop (3*y))
                                     (take 3 (drop (3*x) sud))
-- * D3

-- |
isOkay :: Sudoku -> Bool
isOkay sud = and $ map (isOkayBlock) (blocks sud)

-------------------------------------------------------------------------
