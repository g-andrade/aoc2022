#!/usr/bin/env -S runhaskell -XBangPatterns
-- vim: set expandtab:

import Control.Concurrent
import Data.MultiMap -- requires libghc-multimap-dev package (or equivalent)
import Data.Set
import Debug.Trace
import System.Environment
import System.IO

data Vector = Vector
    { deltaX :: Float
    , deltaY :: Float
    }
    deriving Show

data Direction = Up | Down | Left | Right
    deriving Show

data RopeState = RopeState
    { ropeElements :: [Position]
    , tailVisits :: Set Position
    }

data Position = Position
    { x :: Float
    , y :: Float
    }
    deriving (Show, Eq, Ord)

data ApplyResult =
    MovedHorizOrVert Position Vector |
    MovedDiagonally Position Vector |
    OnlyFirstMoved Position


instance Show RopeState where
   show (RopeState ropeElements tailVisits) = do
        let (eckses, whies) = unzip [(x, y) | (Position x y) <- ropeElements]
        let minX = Prelude.foldl min 99999 eckses
        let maxX = Prelude.foldl max 0 eckses
        let minY = Prelude.foldl min 99999 whies
        let maxY = Prelude.foldl max 0 whies

        let enumeratedElements = zip [0..(length(ropeElements) - 1)] ropeElements
        let elementsPerPositionList = [((x, y), nr) | (nr, Position x y) <- enumeratedElements]
        let elementsPerPosition = Data.MultiMap.fromList elementsPerPositionList

        let matrix = reverse [[Data.MultiMap.lookup (x, y) elementsPerPosition | x <- [minX..maxX]] | y <- [minY..maxY]]
        "\n" ++ (
            "X: " ++ show minX ++ ".." ++ show maxX ++ ", Y:" ++ show minY ++ ".." ++ show maxY ++ "\n") ++ concat [show row ++ "\n" | row <- matrix]

main = do
    args <- System.Environment.getArgs
    let extraRopeLength = read (head args) :: Integer
    let ropeLength = 2 + extraRopeLength

    inputLines <- fmap lines getContents
    debug "input lines" inputLines

    let instructions = Prelude.map parseInstruction inputLines
    debug "instructions" instructions

    let initialPosition = Position 0.0 0.0
    let initialState = RopeState {
          ropeElements = [initialPosition | _ <- [1..ropeLength]]
        , tailVisits = Data.Set.fromList [initialPosition]
        }
    debug "initial state" initialState

    let enumeratedInstructions = zip [1..length instructions] instructions
    let !finalState = Prelude.foldl applyInstruction initialState enumeratedInstructions
    debug "nr of tail visits" (Data.Set.size (tailVisits finalState))

parseInstruction :: String -> Vector
parseInstruction line = do
    case line of
        ('U' : ' ' : amountStr) -> Vector{deltaX = 0.0, deltaY = read amountStr}
        ('D' : ' ' : amountStr) -> Vector{deltaX = 0.0, deltaY = -(read amountStr)}
        ('L' : ' ' : amountStr) -> Vector{deltaX = -(read amountStr), deltaY = 0.0}
        ('R' : ' ' : amountStr) -> Vector{deltaX = read amountStr, deltaY = 0.0}

applyInstruction :: RopeState -> (Int, Vector) -> RopeState
applyInstruction state (instructionNr, instruction) = do
    let unitInstruction = unitVector instruction
    let applyAmount = round (vectorLength instruction)
    let updatedState1 = applyInstructionRecur state instructionNr unitInstruction applyAmount
    -- let updatedState = trace ("state nr" ++ show instructionNr ++ ": " ++ show updatedState1) updatedState1
    let updatedState = updatedState1
    updatedState

applyInstructionRecur :: RopeState -> Int -> Vector -> Int -> RopeState
applyInstructionRecur state instructionNr unitInstruction applyAmount | applyAmount > 0 = do
    let updatedState1 = applyInstructionToState state instructionNr unitInstruction
    -- let updatedState = trace ("state nr" ++ show instructionNr ++ ": " ++ show updatedState1) updatedState1
    let updatedState = updatedState1
    applyInstructionRecur updatedState instructionNr unitInstruction (applyAmount - 1)
applyInstructionRecur state instructionNr unitInstruction 0 = do
    state

applyInstructionToState :: RopeState -> Int -> Vector -> RopeState
applyInstructionToState state instructionNr unitInstruction = do
    let rope = ropeElements state
    let updatedRope = applyInstructionToRope rope instructionNr unitInstruction 1
    let tail = last updatedRope
    state {
        ropeElements = updatedRope,
        tailVisits = Data.Set.insert tail (tailVisits state)
    }

applyInstructionToRope :: [Position] -> Int -> Vector -> Int -> [Position]
applyInstructionToRope (first : second : next) instructionNr unitInstruction elementIndx = do
    case applyInstructionToRopeMembers first second unitInstruction of
        MovedHorizOrVert updatedFirst newUnitInstruction ->
            -- trace (show instructionNr ++ " moved #" ++ show elementIndx ++ " H/V to " ++ show updatedFirst)
            (updatedFirst : (applyInstructionToRope (second : next) instructionNr newUnitInstruction (elementIndx + 1)))
        MovedDiagonally updatedFirst newUnitInstruction ->
            -- trace (show instructionNr ++ " moved #" ++ show elementIndx ++ " diag to " ++ show updatedFirst)
            (updatedFirst : (applyInstructionToRope (second : next) instructionNr newUnitInstruction (elementIndx + 1)))
        OnlyFirstMoved updatedFirst ->
            -- trace (show instructionNr ++ " moved #" ++ show elementIndx ++ " only once to " ++ show updatedFirst ++ "\n----")
            (updatedFirst : second : next)
applyInstructionToRope [last] instructionNr unitInstruction elementIndx = do
    let updatedLast = applyInstructionToPos last unitInstruction
    -- trace (show instructionNr ++ " moved last to " ++ show updatedLast ++ "\n----") [updatedLast]
    [updatedLast]

-- applyInstructionToRopeMembers :: Position -> Position -> Vector -> ApplyResult
applyInstructionToRopeMembers first second unitInstruction = do
    let updatedFirst = applyInstructionToPos first unitInstruction
    let positionalDiff = differenceInPositions updatedFirst second

    case positionalDiff of
        Vector deltaX deltaY | (max (abs deltaX) (abs deltaY) >= 2) && (min deltaX deltaY == 0) -> do
            let newUnitInstruction = unitVector positionalDiff
            MovedHorizOrVert updatedFirst newUnitInstruction
        Vector deltaX deltaY | (max (abs deltaX) (abs deltaY) >= 2) && (abs deltaY > abs deltaX) -> do
            let newUnitInstruction = unitVector unitInstruction{ deltaX = (x updatedFirst) - (x second) }
            MovedDiagonally updatedFirst newUnitInstruction
        Vector deltaX deltaY | (max (abs deltaX) (abs deltaY) >= 2) -> do
            let newUnitInstruction = unitVector unitInstruction{ deltaY = (y updatedFirst) - (y second) }
            MovedDiagonally updatedFirst newUnitInstruction
        _ ->
            OnlyFirstMoved updatedFirst

unitVector :: Vector -> Vector
unitVector (Vector deltaX 0.0) | deltaX /= 0 = do
    (Vector (deltaX / abs deltaX) 0.0)
unitVector (Vector 0.0 deltaY) | deltaY /= 0 = do
    (Vector 0.0 (deltaY / abs deltaY))
unitVector (Vector deltaX deltaY) | deltaX /= 0 && deltaY /= 0 = do
    (Vector (deltaX / abs deltaX) (deltaY / abs deltaY))

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
