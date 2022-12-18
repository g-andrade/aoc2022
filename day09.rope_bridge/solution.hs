#!/usr/bin/env -S runhaskell -XBangPatterns
-- vim: set expandtab:

import Control.Concurrent
import Data.Set
import Debug.Trace
import System.IO

data Vector = Vector
    { deltaX :: Float
    , deltaY :: Float
    }
    deriving Show

data Direction = Up | Down | Left | Right
    deriving Show

data RopeState = RopeState
    { headPos    :: Position
    , tailPos    :: Position
    , tailVisits :: Set Position
    }
    deriving Show

data Position = Position
    { x :: Float
    , y :: Float
    }
    deriving (Show, Eq, Ord)

main = do
    inputLines <- fmap lines getContents
    debug "input lines" inputLines

    let instructions = Prelude.map parseInstruction inputLines
    debug "instructions" instructions

    let initialPosition = Position 0.0 0.0
    let initialState = RopeState {
          headPos = initialPosition
        , tailPos = initialPosition
        , tailVisits = Data.Set.fromList [initialPosition]
        }
    debug "initial state" initialState

    let !finalState = Prelude.foldl applyInstruction initialState instructions
    debug "nr of tail visits" (size (tailVisits finalState))

parseInstruction :: String -> Vector
parseInstruction line = do
    case line of
        ('U' : ' ' : amountStr) -> Vector{deltaX = 0.0, deltaY = read amountStr}
        ('D' : ' ' : amountStr) -> Vector{deltaX = 0.0, deltaY = -(read amountStr)}
        ('L' : ' ' : amountStr) -> Vector{deltaX = -(read amountStr), deltaY = 0.0}
        ('R' : ' ' : amountStr) -> Vector{deltaX = read amountStr, deltaY = 0.0}

applyInstruction :: RopeState -> Vector -> RopeState
applyInstruction state instruction = do
    let unitInstruction = unitVector instruction
    let applyAmount = round (vectorLength instruction)
    applyInstructionRecur state unitInstruction applyAmount

applyInstructionRecur :: RopeState -> Vector -> Integer -> RopeState
applyInstructionRecur state unitInstruction applyAmount | applyAmount > 0 = do
    let updatedState = applyInstructionToState state unitInstruction
    applyInstructionRecur updatedState unitInstruction (applyAmount - 1)
applyInstructionRecur state unitInstruction 0 = do
    state

applyInstructionToState :: RopeState -> Vector -> RopeState
applyInstructionToState state unitInstruction = do
    let updatedHead = applyInstructionToPos (headPos state) unitInstruction
    let tail = (tailPos state)
    let positionalDiff = differenceInPositions updatedHead tail

    case vectorLength positionalDiff >= 2 of
        True | (deltaX positionalDiff) == 0 || (deltaY positionalDiff) == 0 -> do
            let updatedTail = applyInstructionToPos tail unitInstruction
            state {
                headPos = trace ("head moving straight to " ++ show updatedHead) updatedHead,
                tailPos = trace ("tail moving straight to " ++ show updatedTail ++ "\n----") updatedTail,
                tailVisits = Data.Set.insert updatedTail (tailVisits state) }

        True | (abs (deltaX positionalDiff)) < (abs (deltaY positionalDiff)) -> do
            let updatedTail1 = applyInstructionToPos tail unitInstruction
            let updatedTail = updatedTail1 { x = (x updatedHead) }
            state {
                headPos = trace ("head moving vertically to " ++ show updatedHead) updatedHead,
                tailPos = trace ("head accompanying it to " ++ show updatedTail ++ "\n----") updatedTail,
                tailVisits = Data.Set.insert updatedTail (tailVisits state) }

        True -> do
            let updatedTail1 = applyInstructionToPos tail unitInstruction
            let updatedTail = updatedTail1 { y = (y updatedHead) }
            state {
                headPos = trace ("head moving horizontally to " ++ show updatedHead) updatedHead,
                tailPos = trace ("tail accompanying it to " ++ show updatedTail ++ "\n----") updatedTail,
                tailVisits = Data.Set.insert updatedTail (tailVisits state) }

        False ->
            state {
                headPos = trace ("only head moving to " ++ show updatedHead ++ "\n--") updatedHead }

unitVector :: Vector -> Vector
unitVector (Vector deltaX 0.0) | deltaX /= 0 = do
    (Vector (deltaX / abs deltaX) 0.0)
unitVector (Vector 0.0 deltaY) | deltaY /= 0 = do
    (Vector 0.0 (deltaY / abs deltaY))

applyInstructionToPos :: Position -> Vector -> Position
applyInstructionToPos (Position x y) (Vector deltaX deltaY) = do
    Position (x + deltaX) (y + deltaY)

differenceInPositions :: Position -> Position -> Vector
differenceInPositions (Position x1 y1) (Position x2 y2) = do
    Vector {deltaX = (x1 - x2), deltaY = (y1 - y2)}

vectorLength :: Vector -> Float
vectorLength (Vector deltaX deltaY) = do
    sqrt ((deltaX*deltaX) + (deltaY*deltaY))

debug desc x = do
    hPutStrLn stderr (desc ++ " " ++ (show x))
