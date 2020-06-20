module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser
import Browser.Navigation
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Http exposing (Error(..))
import Json.Decode exposing (Decoder, field, list, map2, string)
import Url exposing (Protocol(..))


apiKey =
    "0fc0a5a1dd4723f1e621672ea7ae8b97"


apiBaseUrl =
    "https://trello.com/1"


boardId =
    "LR3ShJNh"


listName =
    "Projects"



-- MAIN


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = \_ -> Never
        , onUrlChange = \_ -> Never
        }



-- MODEL


type Model
    = Unauthorized
    | FragmentError String
    | Token String
    | ListsGetError String
    | LoadingList String
    | FindListError String
    | CardsGetError String
    | CardNames (List String)


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init _ url _ =
    url.fragment
        |> Maybe.map
            (\frag ->
                case String.split "=" frag |> parseTokenFromFragment of
                    Ok token ->
                        ( Token token, getLists apiKey token boardId )

                    Err err ->
                        ( FragmentError err, Cmd.none )
            )
        |> Maybe.withDefault ( Unauthorized, Cmd.none )


type alias FragmentError =
    String


parseTokenFromFragment : List String -> Result FragmentError String
parseTokenFromFragment segments =
    case segments of
        [] ->
            Ok "hiyer"

        [ tokenKey, token ] ->
            if tokenKey == "token" then
                Ok token

            else
                Err "First key of first fragment pair was not 'token'"

        _ ->
            Err "Fragments were not in expected format"


type Msg
    = Never
    | Authorize
      -- tidy this up, bad that it's just strings
    | GetLists String String String
    | ListsReceived String String (Result Http.Error Lists)
    | CardsReceived String String (Result Http.Error Cards)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Never ->
            ( model, Cmd.none )

        Authorize ->
            ( model
            , Browser.Navigation.load
                (apiBaseUrl ++ "/authorize?expiration=1day&name=testing-login&scope=read&response_type=token&key=" ++ apiKey ++ "&return_url=http://localhost:8000")
            )

        GetLists key token id ->
            ( model, getLists key token id )

        ListsReceived key token result ->
            case result of
                Err httpErr ->
                    ( ListsGetError "Error getting boards", Cmd.none )

                Ok lists ->
                    let
                        projectLists =
                            List.filter (\l -> l.name == listName) lists
                    in
                    case projectLists of
                        [] ->
                            ( FindListError "No list found with the project name", Cmd.none )

                        [ list ] ->
                            ( LoadingList list.id, getCards key token list.id )

                        _ ->
                            ( FindListError "Too many lists found with the project name", Cmd.none )

        CardsReceived key token result ->
            case result of
                Err httpErr ->
                    ( CardsGetError "Error getting cards", Cmd.none )

                Ok cards ->
                    ( CardNames (List.map (\c -> c.name) cards), Cmd.none )


getLists : String -> String -> String -> Cmd Msg
getLists key token localBoardId =
    Http.get
        { url = apiBaseUrl ++ "/boards/" ++ localBoardId ++ "/lists?key=" ++ key ++ "&token=" ++ token
        , expect = Http.expectJson (ListsReceived key token) listsDecoder
        }


getCards : String -> String -> String -> Cmd Msg
getCards key token listId =
    Http.get
        { url = apiBaseUrl ++ "/lists/" ++ listId ++ "/cards?key=" ++ key ++ "&token=" ++ token
        , expect = Http.expectJson (CardsReceived key token) cardsDecoder
        }


type alias Lists =
    List TList


type alias TList =
    { id : String
    , name : String
    }


listsDecoder : Decoder Lists
listsDecoder =
    list listDecoder


listDecoder : Decoder TList
listDecoder =
    map2 TList
        (field "id" string)
        (field "name" string)


type alias Cards =
    List Card


type alias Card =
    { id : String
    , name : String
    }


cardsDecoder : Decoder Cards
cardsDecoder =
    list cardDecoder


cardDecoder : Decoder Card
cardDecoder =
    map2 Card
        (field "id" string)
        (field "name" string)



-- VIEW


view : Model -> Browser.Document Msg
view model =
    Browser.Document "Next Actions"
        [ div
            []
            [ case model of
                Unauthorized ->
                    button [ onClick Authorize ] [ text "Authorize" ]

                Token token ->
                    div [] [ text <| apiBaseUrl ++ "/boards/" ++ boardId ++ "/lists?key=" ++ apiKey ++ "&token=" ++ token ]

                FragmentError err ->
                    div [] [ text err ]

                ListsGetError err ->
                    div [] [ text err ]

                LoadingList listId ->
                    div [] [ text listId ]

                FindListError err ->
                    div [] [ text err ]

                CardsGetError err ->
                    div [] [ text err ]

                CardNames names ->
                    div [] (List.map (\n -> div [] [ text n ]) names)
            ]
        ]
