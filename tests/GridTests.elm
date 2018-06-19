module GridTests exposing (all)

import Test exposing (..)
import Expect
import Grid exposing (..)


all : Test
all =
    describe "Grid Module"
        [ test "should create a new matrix" <|
            \_ ->
                let
                    actual =
                        toLists <| new 2 3 (\( x, y ) -> x + y)

                    expected =
                        [ [ 0, 1 ]
                        , [ 1, 2 ]
                        , [ 2, 3 ]
                        ]
                in
                    Expect.equal expected actual
        , test "should get the size of the matrix" <|
            \_ ->
                let
                    actual =
                        size <| new 2 3 (\_ -> 0)

                    expected =
                        ( 2, 3 )
                in
                    Expect.equal expected actual
        , test "should map a grid" <|
            \_ ->
                let
                    actual =
                        new 2 3 (\( x, y ) -> x + y)
                            |> map (\x -> x * 2)
                            |> toLists

                    expected =
                        [ [ 0, 2 ]
                        , [ 2, 4 ]
                        , [ 4, 6 ]
                        ]
                in
                    Expect.equal actual expected
        , test "should get element at index" <|
            \_ ->
                let
                    actual =
                        new 2 3 (\( x, y ) -> x + y)
                            |> get 1 2

                    expected =
                        Just 3
                in
                    Expect.equal expected actual
        , test "should perform an map with location" <|
            \_ ->
                let
                    actual =
                        new 2 3 (\_ -> 0)
                            |> locationMap (\( x, y ) _ -> x + y)
                            |> toLists

                    expected =
                        [ [ 0, 1 ]
                        , [ 1, 2 ]
                        , [ 2, 3 ]
                        ]
                in
                    Expect.equal expected actual
        , test "should set value at a location" <|
            \_ ->
                let
                    actual =
                        new 2 3 (\_ -> 0)
                            |> set 0 1 5
                            |> toLists

                    expected =
                        [ [ 0, 0 ]
                        , [ 5, 0 ]
                        , [ 0, 0 ]
                        ]
                in
                    Expect.equal expected actual
        , test "chunk helper should work" <|
            \_ ->
                let
                    expected =
                        [ [ 1, 2, 3 ], [ 4, 5, 6 ], [ 7, 8, 9 ], [ 10 ] ]

                    actual =
                        chunk 3 (List.range 1 10)
                in
                    Expect.equal expected actual
        ]
