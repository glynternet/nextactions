module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser
import Browser.Navigation
import Html exposing (Html, a, button, div, text)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Url
import Url.Parser exposing ((</>), (<?>), Parser, int, map, oneOf, s, string)



-- MAIN
--main =
--Browser.sandbox { init = init, update = update, view = view }


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = \_ -> Never
        , onUrlChange = \_ -> Never
        }



--{ init : flags -> Url -> Key -> ( model, Cmd msg )
--, update : msg -> model -> ( model, Cmd msg )
--}



---> Program flags model msg
-- MODEL


type alias Model =
    { token : Maybe String
    }


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd msg )
init _ url _ =
    case url.fragment of
        Just routFrag ->
            ( Model (Just routFrag), Cmd.none )

        Nothing ->
            ( Model Nothing, Cmd.none )



--Url.Parser.fragment
--url.fragment
--( 100, Cmd.none )
-- UPDATE


type Msg
    = Never
    | Authorize


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Authorize ->
            ( model, Browser.Navigation.load "https://trello.com/1/authorize?expiration=1day&name=testing-login&scope=read&response_type=token&key=0fc0a5a1dd4723f1e621672ea7ae8b97&return_url=http://localhost:8000" )

        Never ->
            ( model, Cmd.none )


type alias RouteWithFragment =
    ( String, Maybe String )


urlParser : Parser (RouteWithFragment -> a) a
urlParser =
    map Tuple.pair (string </> Url.Parser.fragment identity)



-- VIEW


view : Model -> Browser.Document Msg
view model =
    Browser.Document "Next Actions"
        [ div
            []
            [ model.token
                |> Maybe.map
                    (\token -> div [] [ text token ])
                |> Maybe.withDefault
                    (button [ onClick Authorize ] [ text "Authorize" ])
            ]
        ]
