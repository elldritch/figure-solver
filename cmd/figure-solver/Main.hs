module Main (main) where

import Data.Foldable (foldl)
import Data.List (groupBy)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Options.Applicative (ParserInfo, execParser, fullDesc, help, helper, info, long, metavar, progDesc, strOption)
import Options.Applicative qualified as Flags
import Relude hiding (show)
import Relude.Unsafe qualified as Unsafe
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
  putLn
  traverse_ (printStep puzzle) $ nextSteps puzzle
  print $ viaNonEmpty head $ sortOn length $ solutions puzzle
  where
    putLn = putStrLn ""

    printStep :: Puzzle -> (Int, Puzzle) -> IO ()
    printStep (Puzzle orig) (i, p) = do
      let tile = Unsafe.fromJust $ Unsafe.fromJust $ Map.lookup (i, 0) orig
      putStrLn $ show i <> ": " <> show tile
      print p
      putLn

type Parser = Parsec Void Text

data Tile = Green | Yellow | Purple | White
  deriving stock (Eq)

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

type Position = (Int, Int)

newtype Puzzle = Puzzle (Map Position (Maybe Tile))

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
    rowP :: Int -> Parser [(Position, Maybe Tile)]
    rowP row = (zip [(col, row) | col <- [0 ..]]) <$> replicateM 5 (Just <$> tileP) <* eol

nextSteps :: Puzzle -> [(Int, Puzzle)]
nextSteps (Puzzle p) = catMaybeSnd $ zip [0 .. 4] $ fmap withoutBottomTile [0 .. 4]
  where
    catMaybeSnd :: [(a, Maybe b)] -> [(a, b)]
    catMaybeSnd ((a, Just b) : xs) = (a, b) : catMaybeSnd xs
    catMaybeSnd ((_, Nothing) : xs) = catMaybeSnd xs
    catMaybeSnd [] = []

    withoutBottomTile :: Int -> Maybe Puzzle
    withoutBottomTile i = do
      -- Find tiles to remove.
      connected <- connectedTiles (i, 0)
      -- Remove connected tiles.
      pure $ Puzzle $ foldl (\p' pos -> Map.insert pos Nothing p') p connected
    -- TODO: Shift tiles with gravity.

    connectedTiles :: Position -> Maybe (Set Position)
    connectedTiles pos = do
      tile <- join $ Map.lookup pos p
      executingStateT mempty $ connectedTilesR tile pos

    connectedTilesR :: Tile -> Position -> StateT (Set Position) Maybe ()
    connectedTilesR orig pos = do
      tile <- traceShowWith (\t -> "tile: " <> show t) <<$>> lift $ join $ Map.lookup pos p
      guard $ traceShowWith (\g -> "guard: " <> show g) $ tile == traceShowWith (\o -> "orig: " <> show o) orig
      modify $ Set.insert pos
      seen <- traceShowWith (\s -> "seen: " <> show s) <$> get
      traverse_ (connectedTilesR orig) $
        filter (not . (`Set.member` seen)) $
          traceShowWith (\a -> "adjacents: " <> show a) $ adjacents pos

    adjacents :: Position -> [Position]
    adjacents (x, y) = [(x + dx, y + dy) | dx <- [-1, 1], dy <- [-1, 1]]

solutions :: Puzzle -> [[Int]]
solutions = undefined
