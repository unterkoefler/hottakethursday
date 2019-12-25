module Take exposing (Take, createNewTake, likeOrUnlike, toggleHover)

import Data.User as User exposing (User)
import Time



-- MODEL


type alias Take =
    { content : String
    , postedBy : User
    , timePosted : Time.Posix
    , likedBy : List User
    , hoveredOver : Bool
    }



-- UPDATE


createNewTake : String -> User -> Time.Posix -> Take
createNewTake newTake user time =
    { content = newTake
    , postedBy = user
    , timePosted = time
    , likedBy = []
    , hoveredOver = False
    }


toggleHover : Take -> Take
toggleHover t =
    { t | hoveredOver = not t.hoveredOver }


likeOrUnlike : User -> Take -> Take
likeOrUnlike user take =
    if List.member user take.likedBy then
        { take | likedBy = List.filter (\u -> u /= user) take.likedBy }

    else
        { take | likedBy = user :: take.likedBy }
