module Main (main) where

import Data.List (groupBy)
import Data.Map.Strict qualified as Map
import Options.Applicative (ParserInfo, execParser, fullDesc, help, helper, info, long, metavar, progDesc, strOption)
import Options.Applicative qualified as Flags
import Relude hiding (show)
import Text.Megaparsec (Parsec, eof, errorBundlePretty, runParser)
import Text.Megaparsec.Char (char, eol)
import Text.Show (Show (show))

newtype Options = Options
  { puzzleFile :: FilePath
  }

optionsP :: Flags.Parser Options
optionsP =
  Options
    <$> strOption (long "puzzle" <> metavar "FILE" <> help "File path to puzzle input")

argparser :: ParserInfo Options
argparser = info (optionsP <**> helper) (fullDesc <> progDesc "Solve a figure.game puzzle")

main :: IO ()
main = do
  Options{puzzleFile} <- execParser argparser
  parsed <- runParser puzzleP puzzleFile <$> readFileText puzzleFile
  puzzle <- case parsed of
    Right puzzle -> pure puzzle
    Left err -> putStrLn (errorBundlePretty err) >> exitFailure
  print puzzle
  print $ viaNonEmpty head $ sortOn length $ solutions puzzle

type Parser = Parsec Void Text

data Tile = Green | Yellow | Purple | White

instance Show Tile where
  show Green = "G"
  show Yellow = "Y"
  show Purple = "P"
  show White = "W"

tileP :: Parser Tile
tileP =
  (Green <$ char 'G')
    <|> (Yellow <$ char 'Y')
    <|> (Purple <$ char 'P')
    <|> (White <$ char 'W')

newtype Puzzle = Puzzle (Map (Int, Int) (Maybe Tile))

instance Show Puzzle where
  show (Puzzle p) =
    intercalate "\n" $
      reverse $
        fmap (concatMap (showMTile . snd)) $
          groupBy ((==) `on` snd . fst) $
            sortOn (snd . fst) $ sortOn (fst . fst) $ Map.toList p
    where
      showMTile (Just t) = show t
      showMTile Nothing = " "

puzzleP :: Parser Puzzle
puzzleP = Puzzle . Map.fromList . concat <$> mapM rowP (reverse [0 .. 4]) <* eof
  where
    rowP :: Int -> Parser [((Int, Int), Maybe Tile)]
    rowP row = (zip [(col, row) | col <- [0 ..]]) <$> replicateM 5 (Just <$> tileP) <* eol

nextSteps :: Puzzle -> [(Int, Puzzle)]
nextSteps = undefined

solutions :: Puzzle -> [[Int]]
solutions = undefined
