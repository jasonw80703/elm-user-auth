module Api.Me exposing (User, get)

import Effect exposing (Effect)
import Http
import Json.Decode as Decode


type alias User =
    { id : String
    , name : String
    , profileImageUrl : String
    , email : String
    }


decoder : Decode.Decoder User
decoder =
    Decode.map4 User
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "profileImageUrl" Decode.string)
        (Decode.field "email" Decode.string)


get :
    { onResponse : Result Http.Error User -> msg
    , token : String
    }
    -> Effect msg
get options =
    let
        url : String
        url =
            [ "http://localhost:5000/api/me?token=", options.token ]
                |> String.concat

        cmd : Cmd msg
        cmd =
            Http.get
                { url = url
                , expect = Http.expectJson options.onResponse decoder
                }
    in
    Effect.sendCmd cmd