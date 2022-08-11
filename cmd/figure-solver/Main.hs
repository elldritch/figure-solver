module Main (main) where

import Options.Applicative (ParserInfo, execParser, fullDesc, help, helper, info, long, metavar, progDesc, strOption)
import Options.Applicative qualified as Flags
import Relude
import Text.Megaparsec (Parsec, runParser)
import Text.Megaparsec.Char (char)
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
    Left err -> undefined
  print $ viaNonEmpty head $ sortOn length $ solutions puzzle

type Parser = Parsec Void Text

data Tile = Green | Yellow | Purple | White

tileP :: Parser Tile
tileP =
  (Green <$ char 'G')
    <|> (Yellow <$ char 'Y')
    <|> (Purple <$ char 'P')
    <|> (White <$ char 'W')

newtype Puzzle = Puzzle (Map (Int, Int) (Maybe Tile))

instance Show Puzzle where
  show = undefined

puzzleP :: Parser Puzzle
puzzleP = undefined

nextSteps :: Puzzle -> [(Int, Puzzle)]
nextSteps = undefined

solutions :: Puzzle -> [[Int]]
solutions = undefined
