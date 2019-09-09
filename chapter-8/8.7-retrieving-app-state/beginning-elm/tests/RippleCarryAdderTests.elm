module RippleCarryAdderTests exposing
    ( andGateTests
    , fullAdderTests
    , halfAdderTests
    , inverterTests
    , orGateTests
    , rippleCarryAdderProperty1
    , rippleCarryAdderProperty3
    , rippleCarryAdderTests
    )

import Array
import Expect exposing (Expectation)
import Fuzz exposing (..)
import RippleCarryAdder exposing (..)
import Test exposing (..)


inverterTests =
    describe "Inverter"
        [ test "output is 0 when the input is 1" <|
            \_ ->
                inverter 0
                    |> Expect.equal 1
        , test "output is 1 when the input is 0" <|
            \_ ->
                inverter 1
                    |> Expect.equal 0
        ]


andGateTests =
    describe "AND gate"
        [ test "output is 0 when both inputs are 0" <|
            \_ ->
                andGate 0 0
                    |> Expect.equal 0
        , test "output is 0 when the first input is 0" <|
            \_ ->
                andGate 0 1
                    |> Expect.equal 0
        , test "output is 0 when the second input is 0" <|
            \_ ->
                andGate 1 0
                    |> Expect.equal 0
        , test "output is 1 when both inputs are 1" <|
            \_ ->
                andGate 1 1
                    |> Expect.equal 1
        ]


orGateTests =
    describe "OR gate"
        [ test "output is 0 when both inputs are 0" <|
            \_ ->
                orGate 0 0
                    |> Expect.equal 0
        , test "output is 1 when the first input is 1" <|
            \_ ->
                orGate 0 1
                    |> Expect.equal 1
        , test "output is 1 when the second input is 1" <|
            \_ ->
                orGate 1 0
                    |> Expect.equal 1
        , test "output is 1 when both inputs are 1" <|
            \_ ->
                orGate 1 1
                    |> Expect.equal 1
        ]


halfAdderTests =
    describe "Half adder"
        [ test "sum and carry-out are 0 when both inputs are 0" <|
            \_ ->
                halfAdder 0 0
                    |> Expect.equal { carry = 0, sum = 0 }
        , test "sum is 1 and carry-out is 0 when the 1st input is 0 and the 2nd input is 1" <|
            \_ ->
                halfAdder 0 1
                    |> Expect.equal { carry = 0, sum = 1 }
        , test "sum is 1 and carry-out is 0 when the 1st input is 1 and the 2nd input is 0" <|
            \_ ->
                halfAdder 1 0
                    |> Expect.equal { carry = 0, sum = 1 }
        , test "sum is 0 and carry-out is 1 when both inputs are 1" <|
            \_ ->
                halfAdder 1 1
                    |> Expect.equal { carry = 1, sum = 0 }
        ]


fullAdderTests =
    describe "Full adder"
        [ describe "when both inputs are 0"
            [ describe "and carry-in is 0"
                [ test "both sum and carry-out are 0" <|
                    \_ ->
                        fullAdder 0 0 0
                            |> Expect.equal { carry = 0, sum = 0 }
                ]
            , describe "but carry-out is 1"
                [ test "sum is 1 and carry-out is 0" <|
                    \_ ->
                        fullAdder 0 0 1
                            |> Expect.equal { carry = 0, sum = 1 }
                ]
            ]
        , describe "when the 1st input is 0"
            [ describe "and the 2nd input is 1"
                [ describe "and carry-in is 0"
                    [ test "sum is 1 and carry-out is 0" <|
                        \_ ->
                            fullAdder 0 1 0
                                |> Expect.equal { carry = 0, sum = 1 }
                    ]
                , describe "and carry-in is 1"
                    [ test "sum is 0 and carry-out is 1" <|
                        \_ ->
                            fullAdder 0 1 1
                                |> Expect.equal { carry = 1, sum = 0 }
                    ]
                ]
            ]
        , describe "when the 1st input is 1"
            [ describe "and the 2nd input is 0"
                [ describe "and carry-in is 0"
                    [ test "sum is 1 and carry-out is 0" <|
                        \_ ->
                            fullAdder 1 0 0
                                |> Expect.equal { carry = 0, sum = 1 }
                    ]
                , describe "and carry-in is 1"
                    [ test "sum is 0 and carry-out is 1" <|
                        \_ ->
                            fullAdder 1 0 1
                                |> Expect.equal { carry = 1, sum = 0 }
                    ]
                ]
            , describe "and the 2nd input is 1"
                [ describe "and carry-in is 0"
                    [ test "sum is 0 and carry-out is 1" <|
                        \_ ->
                            fullAdder 1 1 0
                                |> Expect.equal { carry = 1, sum = 0 }
                    ]
                , describe "and carry-in is 1"
                    [ test "sum is 1 and carry-out is 1" <|
                        \_ ->
                            fullAdder 1 1 1
                                |> Expect.equal { carry = 1, sum = 1 }
                    ]
                ]
            ]
        ]


rippleCarryAdderTests =
    describe "4-bit ripple carry adder"
        [ describe "given two binary numbers and a carry-in digit"
            [ test "returns the sum of those numbers and a carry-out digit" <|
                \_ ->
                    rippleCarryAdder 1001 1101 1
                        |> Expect.equal 10111
            ]
        , describe "when the 1st input is 1111, and the 2nd input is 1111"
            [ test "and carry-in is 0, the output is 11110" <|
                \_ ->
                    rippleCarryAdder 1111 1111 0
                        |> Expect.equal 11110
            , test "and carry-in is 1, the output is 11111" <|
                \_ ->
                    rippleCarryAdder 1111 1111 1
                        |> Expect.equal 11111
            ]
        , describe "when the 1st input is 0000, and the 2nd input is 0000"
            [ test "and carry-in is 0, the output is 0000" <|
                \_ ->
                    rippleCarryAdder 0 0 0
                        |> Expect.equal 0
            , test "and carry-in is 1, the output is 0001" <|
                \_ ->
                    rippleCarryAdder 0 0 1
                        |> Expect.equal 1
            ]
        ]


rippleCarryAdderProperty1 : Test
rippleCarryAdderProperty1 =
    describe "carry-out's relationship with most significant digits"
        [ fuzz3
            (list (intRange 0 1))
            (list (intRange 0 1))
            (intRange 0 1)
            "carry-out is 0 when most significant digits are both 0"
          <|
            \list1 list2 carryIn ->
                let
                    convertToBinary digitsList =
                        digitsList
                            |> List.take 3
                            |> numberFromDigits

                    firstInput =
                        convertToBinary list1

                    secondInput =
                        convertToBinary list2
                in
                rippleCarryAdder firstInput secondInput carryIn
                    |> digits
                    |> List.length
                    |> Expect.lessThan 5
        ]


rippleCarryAdderProperty3 : Test
rippleCarryAdderProperty3 =
    describe "carry-in's relationship with least significant digits"
        [ fuzz3
            (list (intRange 0 1))
            (list (intRange 0 1))
            (constant 0)
            """
            the least significant digit of the output is 0 when the
            carry-in is 0 and the least significant digits of both
            inputs are 0
            """
          <|
            \list1 list2 carryIn ->
                let
                    firstInput =
                        convertToBinary list1

                    secondInput =
                        convertToBinary list2

                    convertToBinary digitsList =
                        digitsList
                            |> List.take 4
                            |> setLastDigitToZero
                            |> numberFromDigits

                    setLastDigitToZero digitsList =
                        Array.fromList digitsList
                            |> Array.set (lastIndex digitsList) 0
                            |> Array.toList

                    lastIndex digitsList =
                        List.length digitsList - 1

                    isLastDigitZero digitsList =
                        Array.fromList digitsList
                            |> Array.get (lastIndex digitsList)
                            |> Maybe.withDefault 0
                            |> (==) 0
                in
                rippleCarryAdder firstInput secondInput carryIn
                    |> digits
                    |> isLastDigitZero
                    |> Expect.equal True
        ]
