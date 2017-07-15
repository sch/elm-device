module SlidingBuffer exposing (..)


type alias Size =
    Int


type alias SlidingBuffer a =
    { size : Size
    , items : List a
    }


init : Int -> a -> SlidingBuffer a
init size item =
    SlidingBuffer size (List.singleton item)


toList : SlidingBuffer a -> List a
toList buffer =
    buffer.items


push : a -> SlidingBuffer a -> SlidingBuffer a
push item buffer =
    SlidingBuffer buffer.size (item :: (List.take (buffer.size - 1) buffer.items))


lastEntered : SlidingBuffer a -> Maybe a
lastEntered buffer =
    List.head buffer.items
