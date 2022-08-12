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
      -- Get removed tile.
      let toRemove = (i, 0)
      tile <- join $ Map.lookup toRemove p
      -- Find tiles to remove.
      let connected = connectedTiles tile toRemove
      -- Remove connected tiles.
      let removed = Puzzle $ foldl (\p' pos -> Map.insert pos Nothing p') p connected
      -- Shift tiles with gravity.
      pure $ applyGravity removed

    connectedTiles :: Tile -> Position -> Set Position
    connectedTiles tile pos = executingState mempty $ connectedTilesR tile pos

    connectedTilesR :: Tile -> Position -> State (Set Position) ()
    connectedTilesR orig pos = do
      let tile = join $ Map.lookup pos p
      case tile of
        Just t -> do
          if t == orig
            then do
              modify $ Set.insert pos
              seen <- get
              traverse_ (connectedTilesR orig) $
                filter (not . (`Set.member` seen)) $
                  adjacents pos
            else pass
        Nothing -> pass

    adjacents :: Position -> [Position]
    adjacents (x, y) = [(x + d, y) | d <- [-1, 1]] ++ [(x, y + d) | d <- [-1, 1]]

    applyGravity :: Puzzle -> Puzzle
    applyGravity g = foldl applyGravityCol g [0 .. 4]

    -- Scan upwards from the bottom.
    -- Each empty space adds to the current "gap".
    -- Each tile falls by the current "gap" amount when it's reached.
    applyGravityCol :: Puzzle -> Int -> Puzzle
    applyGravityCol (Puzzle g) col =
      Puzzle $
        fst $
          foldl
            ( \(g', gap) pos@(x, y) -> case Map.lookup pos g of
                Just (Just t) -> (Map.insert (x, y - gap) (Just t) $ Map.insert pos Nothing g', gap)
                Just Nothing -> (g', gap + 1)
                Nothing -> error "impossible: applyGravityCol out-of-bounds"
            )
            (g, 0 :: Int)
            [(col, row) | row <- [0 .. 4]]

solutions :: Puzzle -> [[Int]]
solutions = undefined
