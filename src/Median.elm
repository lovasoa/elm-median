module Median exposing (median)

{-| Compute the median of a list of comparable elements.

# Definition

A median element `m` of a list `l`
is **an element of the list** such that
the difference between the number of elements smaller than `m`
and the number of elements larger than `m` in `l` is minimal.

# Find a median

@docs median

-}


{-|
  Helper function.
  Takes a part of the list, the number of elements before the given part,
  the number of elements after, and returns the median if there are
  elements in the given part of the list.
  Otherwise, it returns `Nothing`.
-}
medianSplit : Int -> Int -> List comparable -> Maybe comparable
medianSplit before after list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            let
                ( smaller, larger ) =
                    List.partition ((>) first) rest

                ( beforeLen, afterLen ) =
                    ( before + List.length smaller, after + List.length larger )

                ( left, right, remains ) =
                    case compare beforeLen afterLen of
                        GT ->
                            ( before, 1 + afterLen, smaller )

                        LT ->
                            ( 1 + beforeLen, after, larger )

                        EQ ->
                            ( 1 + beforeLen, afterLen, [] )
            in
                if remains == [] then
                    Just first
                else
                    medianSplit left right remains


{-| Returns a median element.

The implementation has an average complexity of O(log(N)),
and a worst-case complexity of O(N^2) (when the list is sorted).

If there is a single median element, then it is guarenteed to be returned

    >>> median [1,2,3]
    Just 2

It works with any sortable type.
That is with ints, floats, chars, strings, lists, and tuples.

    >>> median ["Boris", "Arthur", "Adolf", "Jack", "Sarah"]
    Just "Boris"

    >>> median [(0,1), (0,2), (1,0)]
    Just (0,2)

If there is no single median element, it will return an element that most
equally splits the list.

    >>> median [1,1,8,1,1]
    Just 1

    >>> median [1,2,3,4]
    Just 3

If the given list is empty, it will return `Nothing`.

    >>> median []
    Nothing
-}
median : List comparable -> Maybe comparable
median =
    medianSplit 0 0
