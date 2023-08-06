module Api.SignIn exposing
    ( Data, post
    , Error
    )

import Effect exposing (Effect)
import Http
import Json.Decode as Decode
import Json.Encode as Encode


{-|
    Data we expect if the sign in attempt was successful.
-}
type alias Data =
    { token : String
    }


type alias Error =
    { message : String
    , field : Maybe String
    }


{-|
    How to create a `Data` value from JSON.
-}
decoder : Decode.Decoder Data
decoder =
    Decode.map Data
        (Decode.field "token" Decode.string)


{-|
    Sends a POST request to `/api/sign-in` endpoint, returns
    JWT token if user is found with email and password.
-}
post :
    { email : String
    , onResponse : Result (List Error) Data -> msg
    , password : String
    }
    -> Effect msg
post options =
    let
        body : Encode.Value
        body =
            Encode.object
                [ ( "email", Encode.string options.email )
                , ( "password", Encode.string options.password)
                ]

        cmd : Cmd msg
        cmd =
            Http.post
                { url = "http://localhost:5000/api/sign-in"
                , body = Http.jsonBody body
                , expect =
                    Http.expectStringResponse
                        options.onResponse
                        handleHttpResponse
                }
    in
    Effect.sendCmd cmd


handleHttpResponse : Http.Response String -> Result (List Error) Data
handleHttpResponse response =
    case response of
        Http.BadUrl_ _ ->
            Err
                [ { message = "Unexpected URL format"
                  , field = Nothing
                  }
                ]

        Http.Timeout_ ->
            Err
                [ { message = "Request timed out, please try again"
                  , field = Nothing
                  }
                ]

        Http.NetworkError_ ->
            Err
                [ { message = "Could not connect, please try again"
                  , field = Nothing
                  }
                ]

        Http.BadStatus_ { statusCode } body ->
            case Decode.decodeString errorsDecoder body of
                Ok errors ->
                    Err errors

                Err _ ->
                    Err
                        [ { message = "Something unexpected happened" 
                          , field = Nothing
                          }
                        ]

        Http.GoodStatus_ _ body ->
            case Decode.decodeString decoder body of
                Ok data ->
                    Ok data

                Err _ ->
                    Err
                        [ { message = "Something unexpected happened"
                          , field = Nothing
                          }
                        ]


errorsDecoder : Decode.Decoder (List Error)
errorsDecoder =
    Decode.field "errors" (Decode.list errorDecoder)


errorDecoder : Decode.Decoder Error
errorDecoder =
    Decode.map2 Error
        (Decode.field "message" Decode.string)
        (Decode.field "field" (Decode.maybe Decode.string))
