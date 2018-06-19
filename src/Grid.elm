module Grid
    exposing
        ( Grid
        , new
        , size
        , chunk
        , toLists
        , map
        , get
        , locationMap
        , set
        )

import Array exposing (Array)


type alias Size =
    ( Int, Int )


getWidth : Size -> Int
getWidth ( width, _ ) =
    width


getHeight : Size -> Int
getHeight ( _, height ) =
    height


type alias Location =
    ( Int, Int )


type alias Grid a =
    ( Size, Array a )


new : Int -> Int -> (Location -> a) -> Grid a
new width height createCell =
    let
        cols =
            List.range 0 (height - 1)

        row =
            List.range 0 (width - 1)

        matrix =
            Array.fromList <|
                List.concat <|
                    List.map2
                        (\y row -> List.map (\x -> createCell ( x, y )) row)
                        cols
                        (List.repeat height row)
    in
        ( ( width, height ), matrix )


size : Grid a -> Size
size ( size, _ ) =
    size


toLists : Grid a -> List (List a)
toLists ( ( width, _ ), g ) =
    chunk width (Array.toList g)


chunk : Int -> List a -> List (List a)
chunk n list =
    case list of
        [] ->
            []

        x :: xs ->
            List.take n list :: chunk n (List.drop n list)


map : (a -> b) -> Grid a -> Grid b
map f ( size, g ) =
    ( size, Array.map f g )


locationMap : (Location -> a -> b) -> Grid a -> Grid b
locationMap f ( ( width, height ), g ) =
    ( ( width, height )
    , Array.indexedMap
        (\index element -> f ( rem index width, index // width ) element)
        g
    )


get : Int -> Int -> Grid a -> Maybe a
get x y ( ( width, _ ), g ) =
    let
        index =
            width * y + x
    in
        Array.get index g


set : Int -> Int -> a -> Grid a -> Grid a
set x y e ( ( width, height ), g ) =
    ( ( width, height )
    , Array.set (y * width + x) e g
    )
