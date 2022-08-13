module Main (main) where

import Data.Aeson (eitherDecodeStrict, withObject, (.:))
import Data.Aeson.Types (parseEither)
import Data.Foldable (foldl)
import Data.List (groupBy)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (replace)
import Network.HTTP.Req (GET (..), NoReqBody (NoReqBody), bsResponse, defaultHttpConfig, https, req, responseBody, runReq)
import Options.Applicative (ParserInfo, execParser, flag', fullDesc, help, helper, info, long, metavar, progDesc, short, strOption, switch)
import Options.Applicative qualified as Flags
import Relude hiding (show)
import Relude qualified as R
import Relude.Unsafe qualified as Unsafe
import Text.HTML.TagSoup (innerText, parseTags, sections, (~==))
import Text.Megaparsec (Parsec, eof, errorBundlePretty, manyTill, runParser)
import Text.Megaparsec.Char (char, digitChar, eol)
import Text.Show (Show (show))

data PuzzleSource = PuzzleFile FilePath | CurrentPuzzle

puzzleSourceP :: Flags.Parser PuzzleSource
puzzleSourceP =
  (PuzzleFile <$> strOption (long "puzzle" <> short 'f' <> metavar "FILE" <> help "File path to puzzle input"))
    <|> flag' CurrentPuzzle (long "current" <> help "Solve the current online puzzle")

data Options = Options
  { source :: PuzzleSource
  , quiet :: Bool
  }

optionsP :: Flags.Parser Options
optionsP =
  Options
    <$> puzzleSourceP
    <*> switch (long "quiet" <> short 'q' <> help "Only output moves, not intermediate states")

argparser :: ParserInfo Options
argparser = info (optionsP <**> helper) (fullDesc <> progDesc "Solve a figure.game puzzle")

main :: IO ()
main = do
  Options{source, quiet} <- execParser argparser
  (moves, puzzle) <- case source of
    PuzzleFile f -> do
      parsed <- runParser figureP f . decodeUtf8 <$> readFileBS f
      case parsed of
        Right puzzle -> pure puzzle
        Left err -> putStrLn (errorBundlePretty err) >> exitFailure
    CurrentPuzzle -> do
      res <- runReq defaultHttpConfig $ req GET (https "figure.game") NoReqBody bsResponse mempty
      let page = parseTags $ responseBody res
      puzzleJSON <- case sections (~== ("<script id=\"__NEXT_DATA__\">" :: String)) page of
        [element] -> pure $ innerText element
        _ -> error "could not parse https://figure.game website"
      v <- liftEither $ eitherDecodeStrict puzzleJSON
      (tiles, moves) :: ([Int], Int) <- liftEither $
        flip parseEither v $
          withObject "puzzle" $ \o -> do
            props <- o .: "props" >>= (.: "pageProps")
            (,) <$> props .: "tiles" <*> props .: "moves"
      pure (moves, Puzzle $ Map.fromList $ zip ((\n -> (n `mod` 5, 4 - n `div` 5)) <$> [0 ..]) $ Just . toTile <$> tiles)
      where
        toTile :: Int -> Tile
        toTile 0 = Green
        toTile 1 = Purple
        toTile 2 = Yellow
        toTile 3 = White
        toTile n = error $ "could not parse tile value: " <> R.show n

  putStrLn $ "Solving puzzle in " <> show moves <> " moves:"
  putTextLn $ showPuzzle puzzle
  putLn

  putStrLn "Solution:"
  let solution =
        fromMaybe (error "puzzle has no solution") $
          viaNonEmpty head $
            sortOn length $ solutions moves puzzle
  if quiet
    then print $ fst <$> solution
    else printMoves puzzle solution
  where
    putLn :: IO ()
    putLn = putStrLn ""

    printMoves :: Puzzle -> [(Int, Puzzle)] -> IO ()
    printMoves puzzle moves = void $ foldlM printMovesF puzzle moves

    printMovesF :: Puzzle -> (Int, Puzzle) -> IO Puzzle
    printMovesF (Puzzle prev) (i, p) = do
      let tile = Unsafe.fromJust $ Unsafe.fromJust $ Map.lookup (i, 0) prev
      putStrLn $ show i <> ": " <> show tile
      putTextLn $ showPuzzle p
      putStrLn $ " " <> replicate i ' ' <> "^"
      putLn
      pure p

    showPuzzle :: Puzzle -> Text
    showPuzzle p =
      "+-----+\n"
        <> "|"
        <> shown
        <> "|\n"
        <> "+-----+"
      where
        shown = replace "\n" "|\n|" $ R.show p

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

figureP :: Parser (Int, Puzzle)
figureP = (,) <$> movesP <*> puzzleP <* eof
  where
    movesP :: Parser Int
    movesP = digitChar `manyTill` eol >>= liftEither . readEither

    puzzleP :: Parser Puzzle
    puzzleP = Puzzle . Map.fromList . concat <$> mapM rowP (reverse [0 .. 4])

    rowP :: Int -> Parser [(Position, Maybe Tile)]
    rowP row = (zip [(col, row) | col <- [0 ..]]) <$> replicateM 5 (Just <$> tileP) <* eol

nextSteps :: Puzzle -> [(Int, Puzzle)]
nextSteps (Puzzle puzzle) = catMaybeSnd $ zip [0 .. 4] $ fmap withoutBottomTile [0 .. 4]
  where
    catMaybeSnd :: [(a, Maybe b)] -> [(a, b)]
    catMaybeSnd ((a, Just b) : xs) = (a, b) : catMaybeSnd xs
    catMaybeSnd ((_, Nothing) : xs) = catMaybeSnd xs
    catMaybeSnd [] = []

    withoutBottomTile :: Int -> Maybe Puzzle
    withoutBottomTile i = do
      -- Get removed tile.
      let toRemove = (i, 0)
      tile <- join $ Map.lookup toRemove puzzle
      -- Find tiles to remove.
      let connected = connectedTiles tile toRemove
      -- Remove connected tiles.
      let removed = foldl (\g' pos -> Map.insert pos Nothing g') puzzle connected
      -- Shift tiles with gravity.
      pure $ applyGravity $ Puzzle removed

    connectedTiles :: Tile -> Position -> Set Position
    connectedTiles tile pos = executingState mempty $ connectedTilesR tile pos

    connectedTilesR :: Tile -> Position -> State (Set Position) ()
    connectedTilesR orig pos =
      case join $ Map.lookup pos puzzle of
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
    applyGravity p = foldl applyGravityCol p [0 .. 4]

    -- Scan upwards from the bottom.
    -- Each empty space adds to the current "gap".
    -- Each tile falls by the current "gap" amount when it's reached.
    applyGravityCol :: Puzzle -> Int -> Puzzle
    applyGravityCol (Puzzle p) col =
      Puzzle $
        fst $
          foldl
            ( \(p', gap) pos@(x, y) -> case Map.lookup pos p of
                Just (Just t) -> (Map.insert (x, y - gap) (Just t) $ Map.insert pos Nothing p', gap)
                Just Nothing -> (p', gap + 1)
                Nothing -> error "impossible: applyGravityCol out-of-bounds"
            )
            (p, 0 :: Int)
            [(col, row) | row <- [0 .. 4]]

type Moves = [(Int, Puzzle)]

solutions :: Int -> Puzzle -> [Moves]
solutions moves = fmap reverse . solutionsR []
  where
    solutionsR :: Moves -> Puzzle -> [Moves]
    solutionsR prev g
      | length prev > moves = []
      | solved g = [prev]
      | otherwise = concatMap (\(m, g') -> solutionsR ((m, g') : prev) g') $ nextSteps g

    solved :: Puzzle -> Bool
    solved (Puzzle p) = all isNothing p

liftEither :: (MonadFail m, ToString l) => Either l r -> m r
liftEither = \case
  Left l -> fail $ toString l
  Right r -> pure r
