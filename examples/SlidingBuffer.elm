module SlidingBuffer exposing (..)


type alias Size =
    Int


type SlidingBuffer a
    = SlidingBuffer Size (List a)


init : Int -> a -> SlidingBuffer a
init size item =
    SlidingBuffer size (List.singleton item)


toList : SlidingBuffer a -> List a
toList buffer =
    case buffer of
        SlidingBuffer _ items ->
            items


append : SlidingBuffer a -> a -> SlidingBuffer a
append buffer item =
    case buffer of
        SlidingBuffer size items ->
            SlidingBuffer size (item :: (List.take 100 items))
