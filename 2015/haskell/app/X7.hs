import Lude
import qualified Data.Map.Strict as Map
import Data.Char (isAsciiLower)
import Data.Bits (Bits(shiftR, complement, (.|.), (.&.), shiftL))
import Data.Word (Word16)

data Wire = W String | S Word16 deriving (Eq, Show)

data Cmd
  = Not
  | Or     Wire
  | And    Wire
  | LShift Wire
  | RShift Wire
  | PassThrough
  deriving (Show, Eq)

type Out = String
type Instruction = (Wire, Cmd, Out)
type Parser = Parsec Void String

getInstruction :: Parser Instruction
getInstruction = do
  n <- optional $ string "NOT "
  w <- wire
  cmd <- maybe parseCmd (const $ pure Not) n
  output <- string " -> " >> lowerCase
  pure (w, cmd, output)

  where
    lowerCase = some $ satisfy isAsciiLower
    signal = read <$> some digitChar
    wire = try $ W <$> lowerCase <|> S <$> signal
    parseCmd =
      choice (try <$>
        [ Or     <$> (string " OR "     *> wire)
        , And    <$> (string " AND "    *> wire)
        , LShift <$> (string " LSHIFT " *> wire)
        , RShift <$> (string " RSHIFT " *> wire) ])
        <|> pure PassThrough

runInstruction :: Map.Map String Word16 -> Instruction -> Map.Map String Word16
runInstruction g (w, cmd, o) =
  let setOutput x = g & at o ?~ x
      getWire (W x) = g ^. at x . non (0 :: Word16)
      getWire (S s) = s
      w1 = getWire w
  in setOutput $ case cmd of
    PassThrough  -> w1
    Not          -> complement w1
    Or     w2    -> w1 .|. getWire w2
    And    w2    -> w1 .&. getWire w2
    LShift w2    -> w1 `shiftL` fromIntegral (getWire w2)
    RShift w2    -> w1 `shiftR` fromIntegral (getWire w2)

main :: IO ()
main = do
  input <- lines <$> readFile "inputs/7"

  let input = [ "123 -> x" , "456 -> y" , "x AND y -> d" , "x OR y -> e" , "x LSHIFT 2 -> f" , "y RSHIFT 2 -> g" , "NOT x -> h" , "NOT y -> i" ]

  -- part one
  print $ map (parse getInstruction "") input & rights
        & foldl runInstruction Map.empty
        -- & Map.map (fromIntegral :: Int -> Word16)
        -- & Map.lookup "a"
  -- mapM_ print $ map (parse getOp "") input & lefts



















-- data Instruction 
--     = Signal Int Out 
--     | Shuffle Wire Out
--     | And Wire Wire Out 
--     | Or Wire Wire Out 
--     | LShift Wire Int Out 
--     | RShift Wire Int Out 
--     | Not Wire Out 
--     deriving (Show, Eq)
-- 
-- wire :: Parser String
-- wire = some $ satisfy isAsciiLower
-- 
-- output :: Parser String
-- output = string " -> " >> wire
-- 
-- number :: Parser Int
-- number = read <$> some digitChar
-- 
-- andOp :: Parser Instruction
-- andOp = And 
--     <$> (wire <|> some digitChar)
--     <*> (string " AND " 
--      *> wire)
--     <*> output
-- 
-- orOp :: Parser Instruction
-- orOp = Or <$> wire <*> (string " OR " *> wire) <*> output
-- 
-- notOp :: Parser Instruction
-- notOp = Not <$> (string "NOT " >> wire) <*> output
-- 
-- signal :: Parser Instruction
-- signal = Signal <$> number <*> output
-- 
-- shuffle :: Parser Instruction
-- shuffle = Shuffle <$> wire <*> output
-- 
-- lshift :: Parser Instruction
-- lshift = LShift <$> wire <*> (string " LSHIFT " *> number) <*> output
-- 
-- rshift :: Parser Instruction
-- rshift = RShift <$> wire <*> (string " RSHIFT " *> number) <*> output
-- 
-- op :: Parser Instruction
-- op = choice $ try <$> [andOp, signal, notOp, orOp, lshift, rshift, shuffle]
-- 
-- main :: IO ()
-- main = do 
--   input <- lines <$> readFile "inputs/7"
-- 
--   -- part one

--- getInstruction :: Parser Instruction
-- getInstruction = do
--   (w, cmd) <- try parseNot <|> getCmd
--   output <- string " -> " >> lowerCase
--   pure (w, cmd, output)
-- 
--   where 
--     lowerCase = some $ satisfy isAsciiLower
-- 
--     signal = read <$> some digitChar
-- 
--     wire = try $ W <$> lowerCase <|> S <$> signal
-- 
--     parseNot :: Parser (Wire, Cmd)
--     parseNot = do
--         _ <- string "NOT " 
--         w <- wire
--         pure (w, Not)
-- 
--     getCmd :: Parser (Wire, Cmd)
--     getCmd = do
--         w <- wire
--         cmd <- choice (try <$> 
--                  [ Or     <$> (string " OR "     *> wire) 
--                  , And    <$> (string " AND "    *> wire)
--                  , LShift <$> (string " LSHIFT " *> wire)
--                  , RShift <$> (string " RSHIFT " *> wire) ])
--                 <|> pure PassThrough
--         pure (w, cmd)-   print $ map (parse op "") input & rights & length