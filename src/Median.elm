module Median exposing (median)

{-| Compute the median of a list of comparable elements.

# Definition

A median element `m` of a list `l`
is **an element of the list** such that
the difference between the number of elements smaller than `m`
and the number of elements larger than `m` in `l` is minimal.

# Find a madian

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
        (smaller, larger) = List.partition ((<) first) rest
        (beforeLen, afterLen) = (before + List.length smaller, after + List.length larger)
      in
        case compare beforeLen afterLen of
        GT ->
          medianSplit before (1+afterLen) smaller
        LT ->
          medianSplit (1+beforeLen) after larger
        EQ ->
          Just first


{-|
  Returns a median element.

    >>> median [1,2,3]
    Just 2

    >>> median [1,2,3,4]
    Just 3

    >>> median []
    Nothing
-}
median : List comparable -> Maybe comparable
median = medianSplit 0 0
