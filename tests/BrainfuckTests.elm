module BrainfuckTests exposing (..)

import Test exposing (..)
import Expect

import Brainfuck exposing (..)


-- @TODO: split all the tests into small test suites
defaultMachine : Machine
defaultMachine = 
    { 
        pointer = 0, 
        stack = [], 
        memory = [ 0 ], 
        programCounter = 0, 
        program = [], 
        output = [], 
        input = []
    }


tokemizerTestExpectedTokens : List Token
tokemizerTestExpectedTokens =
    [ Add, Sub, LpStart, LpEnd, PtrLeft, PtrRight, Print, Read ]


sampleProgram1 : String
sampleProgram1 = """
    >++++++++[<+++++++++>-]<.>++++[<+++++++>-]<+.+++++++..+++.>>++++++[<+++++++>-]<+
    +.------------.>++++++[<+++++++++>-]<+.<.+++.------.--------.>>>++++[<++++++++>-
    ]<+.
"""


sampleProgram2 : String
sampleProgram2 = """
    ++++++++ [ > ++++++++ <-] >+ # 1
    >> +++++ [ < +++++ > -] <+
    [ <.+ >- ]
"""


sampleProgram3 : String
sampleProgram3 = """
    ++++++
    [ > ++++++++++ < -]
    >++++<
    ++++
    [ > +. < -]
"""


outputBufferToString : Machine -> String
outputBufferToString m = 
    m.output
    |> List.map (\x -> Char.fromCode  x)
    |> String.fromList


all : Test
all =
    describe "A Test Suite"
        [ test "tokemizer test" <|
            \_ ->
                Expect.equal tokemizerTestExpectedTokens (tokemize "+-[]<>.,")


        -- Machine, memory and stack tests
        , test "creating a new memory" <|
            \_ -> 
                Expect.equal [ 0 ] newMemory


        , test "creating a new stack" <|
            \_ -> 
                Expect.equal [] newStack


        , test "creating a new machine" <|
            \_ -> 
                Expect.equal { defaultMachine | program = [ Add ] } (newMachine [ Add ])


        -- Token Tests
        , test "+ to token" <|
            \_ -> 
                Expect.equal Add (toToken '+')


        , test "- to token" <|
            \_ -> 
                Expect.equal Sub (toToken '-')


        , test "[ to token" <|
            \_ -> 
                Expect.equal LpStart (toToken '[')
        
        
        , test "] to token" <|
            \_ -> 
                Expect.equal LpEnd (toToken ']')
        
        
        , test ". to token" <|
            \_ -> 
                Expect.equal Print (toToken '.')
        
        
        , test ", to token" <|
            \_ -> 
                Expect.equal Read (toToken ',')
        
        
        , test "number to token" <|
            \_ -> 
                Expect.equal NoOp (toToken '3')
        
        
        , test "letter to token" <|
            \_ -> 
                Expect.equal NoOp (toToken 'x')
        
        
        , test "symbol to token" <|
            \_ -> 
                Expect.equal NoOp (toToken '#')
        
        
        -- Operation test
        , test "add operation" <|
            \_ -> 
                Expect.equal { defaultMachine | memory = [ 1 ], programCounter = 1 } (numberOperation  add (newMachine []))
        
        , test "sub operation" <|
            \_ -> 
                Expect.equal { defaultMachine | memory = [ -1 ], programCounter = 1 } (numberOperation sub (newMachine []))
        

        , test "realloc memory" <|
            \_ -> Expect.equal [ 0, 0 ] (realloc [ 0 ])


        , test "needs realloc" <|
            \_ -> Expect.equal True (needsRealloc Right defaultMachine)


        , test "don't needs realloc" <|
            \_ -> Expect.equal False (needsRealloc 
                Right 
                { defaultMachine | pointer = 0, memory = [ 0, 0, 0 ], programCounter = 1})
        
        
        , test "move pointer right operation when memory slot does exists" <|
            \_ -> Expect.equal 
                { defaultMachine | memory = [ 0, 0, 0], pointer = 1, programCounter = 1 } 
                (movePointerOperation Right { defaultMachine | pointer = 0, memory = [ 0, 0, 0 ]})
        
        
        , test "move pointer right operation when memory slot doesn't exists" <|
            \_ -> Expect.equal 
                { defaultMachine | memory = [ 0, 0 ], pointer = 1, programCounter = 1 }
                (movePointerOperation Right defaultMachine) 
        
        
        , test "move pointer left operation" <|
            \_ -> Expect.equal 
                { defaultMachine | memory = [ 0 ], pointer = 1, programCounter = 1 } 
                (movePointerOperation Left ({ defaultMachine | pointer = 2 }))
        
        
        , test "move pointer left operation when pointer is 0" <|
            \_ -> Expect.equal 
                { defaultMachine | memory = [ 0 ], pointer = 0, programCounter = 1 } 
                (movePointerOperation Left ({ defaultMachine | pointer = 0 }))   
        
        
        , test "move pointer left operation when pointer is 1" <|
            \_ -> Expect.equal 
                { defaultMachine | memory = [ 0 ], pointer = 0, programCounter = 1 } 
                (movePointerOperation Left ({ defaultMachine | pointer = 1 }))


        , test "stack push" <|
            \_ -> Expect.equal [ 10 ] (pushStack [] 10)


        , test "stack pop" <|
            \_ -> Expect.equal [] (pop [ 10 ])


        , test "push loop to machine stack" <|
            \_ -> Expect.equal 
                { defaultMachine | stack = [ 0 ], programCounter = 1, program = [ LpStart ] } 
                (pushLoop { defaultMachine | program = [ LpStart ] })


        , test "pop loop to machine stack" <|
            \_ -> Expect.equal 
                { defaultMachine | stack = [], programCounter = 2, program = [ LpStart, LpEnd ] } 
                (popLoop { defaultMachine | stack = [ 0 ], programCounter = 1, program = [ LpStart, LpEnd ] })


        , test "last list item" <|
            \_ -> Expect.equal 0 (Maybe.withDefault -1 (last [ 0 ]))


        , test "set memory in cell" <|
            \_ -> Expect.equal [ 12 , 0] (setMemoryAt 0 12 [ 0, 0 ] )


        , test "write output" <|
            \_ -> Expect.equal 
                { defaultMachine | output = [ 0 ], programCounter = 1 }
                (io Output defaultMachine)


        , test "read input" <|
            \_ -> Expect.equal 
                { defaultMachine | input = [], memory = [ 12 ], programCounter = 1 }
                (io Input { defaultMachine | input = [ 12 ] })


        , test "machine loop test 1" <|
            \_ -> Expect.equal
                "Hello, World!"
                (outputBufferToString (machineLoop (newMachine (tokemize sampleProgram1))))


        , test "machine loop test 2" <|
            \_ -> Expect.equal
                "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                (outputBufferToString (machineLoop (newMachine (tokemize sampleProgram2))))


        , test "machine loop test 3" <|
            \_ -> Expect.equal
                "ABCD"
                (outputBufferToString (machineLoop (newMachine (tokemize sampleProgram3))))
        ]
