{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE LambdaCase #-}

module Main where

--------------------------------------------------------------------------------

import           Control.Monad (join)
import           Data.Char     (isSpace)
import           Data.List     (elemIndex, intercalate, mapAccumL)
import qualified Data.Map      as M
import           Data.Maybe    (fromJust, fromMaybe)
import           Text.Read     (readMaybe)

import           Prelude       hiding (lookup)

--------------------------------------------------------------------------------

-- The fact that this is not in `base` is just stupid.
nth :: [a] -> Int -> Maybe a
nth []      _ = Nothing
nth (h : _) 0 = Just h
nth (_ : t) n = nth t $! (n - 1)

lookup :: (Show a, Show b, Ord a) => a -> M.Map a (Maybe b) -> Maybe b
lookup a m = join (M.lookup a m)

--------------------------------------------------------------------------------

type Instr  = String
type Reg    = String
type RegMap = M.Map Reg (Maybe Int)

data Op = Imm Int | Reg Reg | Stack Int
  deriving (Show)

--------------------------------------------------------------------------------

data InstrHandler
  = OneOp (Op ->       RegMap -> RegMap)
  | TwoOp (Op -> Op -> RegMap -> RegMap)

movq, addq :: InstrHandler

movq = TwoOp $ \op1 op2 regMap ->
  case (op1, op2) of
    (Imm s, Reg d) -> M.insert d (Just s) regMap
    (Reg s, Reg d) -> M.insert d (lookup s regMap) regMap

addq = TwoOp $ \op1 op2 regMap ->
  case (op1, op2) of
    (Imm i, Reg d) -> M.insert d (fmap (+ i) (lookup d regMap)) regMap
    (Reg s, Reg d) -> M.insert d ((+) <$> lookup d regMap <*> lookup s regMap) regMap

subq = TwoOp $ \op1 op2 regMap ->
  case (op1, op2) of
    (Imm i, Reg d) -> M.insert d (fmap (\x -> x - i) (lookup d regMap)) regMap
    (Reg s, Reg d) -> M.insert d ((-) <$> lookup d regMap <*> lookup s regMap) regMap

negq = OneOp $ \(Reg d) regMap ->
  M.insert d ((\x -> (- x)) <$> lookup d regMap) regMap

instrHandlers :: M.Map Instr InstrHandler
instrHandlers = M.fromList
    [ ( "movq", movq )
    , ( "addq", addq )
    , ( "subq", subq )
    , ( "negq", negq )
    ]

--------------------------------------------------------------------------------

parseOp :: String -> Maybe Op
parseOp ('$' : rest) = Imm <$> readMaybe rest
parseOp ('%' : reg ) = Just (Reg reg)
parseOp _            = Nothing

showMapping :: (Reg, Maybe Int) -> String
showMapping (reg, val) = reg ++ " = " ++ showVal val

showVal :: Show a => Maybe a -> String
showVal Nothing = "?"
showVal (Just i) = show i

renderRegMap :: RegMap -> String
renderRegMap = intercalate ", " . map showMapping . M.toList

dropComments :: String -> String
dropComments str
  | Just i <- elemIndex '#' str
  = reverse (dropWhile isSpace (reverse (take i str)))
  | otherwise
  = str

annotateRegs :: RegMap -> String -> (RegMap, String)
annotateRegs regMap str = fromMaybe (regMap, str) $ do
    let ws = words str
    instr_w <- nth ws 0
    regMap' <-
      M.lookup instr_w instrHandlers >>= \case
        TwoOp instr_handler -> do
          op1 <- nth ws 1 >>= parseOp . init
          op2 <- nth ws 2 >>= parseOp
          return (instr_handler op1 op2 regMap)
        OneOp instr_handler -> do
          op <- nth ws 1 >>= parseOp
          return (instr_handler op regMap)
    return (regMap', dropComments str ++ "\t\t# " ++ renderRegMap regMap')

main :: IO ()
main = do
    input <- getContents
    putStr (unlines (snd (mapAccumL annotateRegs M.empty (lines input))))
