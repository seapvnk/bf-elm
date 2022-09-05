module Brainfuck exposing (..)

import List exposing (map)
import String exposing (toList)
import Array exposing (fromList, get)
import List exposing (indexedMap, head, length, take, drop)
import Maybe exposing (withDefault)
import Loop exposing (while)
import List exposing (filter)


type Token 
    = Add 
    | Sub
    | LpStart
    | LpEnd
    | Print
    | Read
    | PtrRight
    | PtrLeft
    | NoOp


add : Int
add = 
    1

sub : Int
sub = 
    -1


type PointerOperation 
    = Left 
    | Right


type IO
    = Input
    | Output


type alias Stack =
    List Int


type alias Pointer =
    Int


type alias Memory =
    List Int


type alias Machine =
    { 
        pointer: Pointer, 
        stack: Stack, 
        memory: Memory, 
        programCounter: Pointer, 
        program: Program,
        output: List Int,
        input: List Int
    }


type alias Program =
    List Token


newStack : Stack
newStack = 
    []


pushStack : Stack -> Int -> Stack
pushStack stack n =
    stack ++ [ n ]


pop : List Int -> List Int
pop stack =
    take ((length stack) - 1) stack


last : List Int -> Maybe Int
last stack =
    drop ((length stack) - 1) stack
    |> head


newMemory : Memory
newMemory =
    [ 0 ]


memoryAt : Int -> Memory -> Maybe Int
memoryAt index memory =
    fromList memory 
    |> get index


setMemoryAt : Int -> Int -> Memory -> Memory
setMemoryAt pointerAt value memory =
    memory 
    |> indexedMap
        (\index element -> choose (index == pointerAt) value element)


newMachine : Program -> Machine 
newMachine program = 
    { 
        pointer = 0, 
        stack = newStack, 
        memory = newMemory, 
        programCounter = 0, 
        program = program,
        output = [],
        input = []
    }


toToken : Char -> Token
toToken chr =
    case chr of
        '+' -> Add

        '-' -> Sub

        '[' -> LpStart

        ']' -> LpEnd

        '.' -> Print

        ',' -> Read

        '>' -> PtrRight

        '<' -> PtrLeft

        _ -> NoOp


tokemize : String -> List Token
tokemize str =
    toList str 
    |> map toToken
    |> filter (\t -> t /= NoOp)


tokenOrNothing : Maybe Token -> Token
tokenOrNothing token = 
    case token of
        Just t -> t
        Nothing -> NoOp


findOperation : Machine -> Maybe Token
findOperation machine = 
    fromList (machine.program)
    |> get (machine.programCounter)


operation : Machine -> Token
operation machine =
    findOperation machine
    |> tokenOrNothing


choose : Bool -> a -> a -> a
choose condition a b =
    if condition then
        a
    else
        b


numberOperation : Int -> Machine -> Machine
numberOperation n machine =
    { 
        machine | 
        memory = machine.memory 
            |> indexedMap 
                (\index element -> choose (index == machine.pointer) (element + n) element),
        programCounter = machine.programCounter + 1
    }


needsRealloc : PointerOperation -> Machine -> Bool
needsRealloc op machine =
    (length machine.memory) <= (machine.pointer + 1) && op == Right


realloc : Memory -> Memory
realloc mem =
    mem ++ [ 0 ]


movePointerOperation : PointerOperation -> Machine -> Machine
movePointerOperation op machine =
    case op of
        Right ->
            { 
                machine | 
                memory = choose (needsRealloc op machine) (realloc machine.memory) machine.memory,
                pointer = machine.pointer + 1,
                programCounter = machine.programCounter + 1
            }


        Left ->
            { 
                machine | 
                pointer = choose (machine.pointer - 1 >= 0) (machine.pointer - 1) machine.pointer,
                programCounter = machine.programCounter + 1
            }


pushLoop : Machine -> Machine
pushLoop machine =
    {
        machine |
        stack = pushStack machine.stack machine.programCounter,
        programCounter = machine.programCounter + 1
    }


continueLoop : Machine -> Bool
continueLoop machine =
    case (memoryAt machine.pointer machine.memory) of
        Just x -> 
            x == 0


        Nothing ->
            False


popLoop : Machine -> Machine
popLoop machine =
    case (last machine.stack) of
        Just pc -> 
            {
                machine |
                stack = pop machine.stack,
                programCounter = choose (continueLoop machine) (machine.programCounter + 1) pc
            }


        Nothing -> 
            machine


io : IO -> Machine -> Machine
io op machine =
    case op of
        Output ->
            case (memoryAt machine.pointer machine.memory) of
                Just x -> 
                    {
                        machine |
                        output = machine.output ++ [ x ],
                        programCounter = machine.programCounter + 1
                    }

                Nothing ->
                    { machine | programCounter = machine.programCounter + 1 }

        Input ->
            {
                machine |
                input = pop machine.input,
                programCounter = machine.programCounter + 1,
                memory = 
                    setMemoryAt machine.pointer (withDefault -1 (last machine.input)) machine.memory
            }


next : Machine -> Machine
next machine =
    case (operation machine) of
        Add -> 
            numberOperation add machine

        Sub -> 
            numberOperation sub machine

        LpStart -> 
            pushLoop machine
        
        LpEnd -> 
            popLoop machine
        
        PtrLeft -> 
            movePointerOperation Left machine
        
        PtrRight -> 
            movePointerOperation Right machine
        
        Print -> 
            io Output machine
        
        Read -> 
            io Input machine
        
        NoOp -> 
            { machine | programCounter = machine.programCounter + 1 }


machineLoop : Machine -> Machine
machineLoop machine =
    if machine.programCounter == (length machine.program)
    then machine
    else machineLoop (next machine)

