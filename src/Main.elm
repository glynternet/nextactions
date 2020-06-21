module Main exposing (..)

import Browser
import Browser.Navigation
import Dict exposing (Dict)
import Html exposing (Html, br, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http exposing (Error(..))
import Json.Decode as Decode exposing (Decoder, field, list, map2, string)
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
    | CardChecklists Cards (Dict String Checklists)


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init _ url _ =
    url.fragment
        |> Maybe.map
            (\frag ->
                case String.split "=" frag |> parseTokenFromFragment of
                    Ok token ->
                        ( Token token, getLists (RequestCredentials apiKey token) boardId )

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
    | GetLists RequestCredentials String
    | ListsReceived RequestCredentials (Result Http.Error Lists)
    | CardsReceived RequestCredentials (Result Http.Error Cards)
    | ChecklistsReceived String (Result Http.Error Checklists)


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

        GetLists credentials id ->
            ( model, getLists credentials id )

        ListsReceived credentials result ->
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
                            ( LoadingList list.id, getCards credentials list.id )

                        _ ->
                            ( FindListError "Too many lists found with the project name", Cmd.none )

        CardsReceived credentials result ->
            case result of
                Err httpErr ->
                    ( CardsGetError <| "Error getting cards: " ++ httpErrToString httpErr, Cmd.none )

                Ok cards ->
                    ( CardChecklists cards Dict.empty
                    , Cmd.batch (List.map (\c -> getChecklists credentials c.id) cards)
                    )

        ChecklistsReceived cardId result ->
            case model of
                CardChecklists cards cls ->
                    case Debug.log ("Received: " ++ cardId) result of
                        Ok newChecklists ->
                            ( CardChecklists cards (Dict.insert cardId newChecklists cls), Cmd.none )

                        Err error ->
                            ( model, Cmd.none )

                _ ->
                    -- TODO: add unexpected error state?
                    ( model, Cmd.none )


getLists : RequestCredentials -> String -> Cmd Msg
getLists credentials localBoardId =
    getItems credentials
        ("/boards/" ++ localBoardId ++ "/lists")
        (ListsReceived credentials)
        listsDecoder


getCards : RequestCredentials -> String -> Cmd Msg
getCards credentials listId =
    getItems credentials
        ("/lists/" ++ listId ++ "/cards")
        (CardsReceived credentials)
        cardsDecoder


getChecklists : RequestCredentials -> String -> Cmd Msg
getChecklists credentials cardId =
    getItems credentials
        ("/cards/" ++ cardId ++ "/checklists")
        (ChecklistsReceived cardId)
        checklistsDecoder


cretentialsParams : RequestCredentials -> String
cretentialsParams credentials =
    "key=" ++ credentials.key ++ "&token=" ++ credentials.token


getItems : RequestCredentials -> String -> (Result Error a -> Msg) -> Decode.Decoder a -> Cmd Msg
getItems credentials endpoint toMsg decoder =
    Http.get
        { url = apiBaseUrl ++ endpoint ++ "?" ++ cretentialsParams credentials
        , expect = Http.expectJson toMsg decoder
        }


type alias RequestCredentials =
    { key : String
    , token : String
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


type alias Checklists =
    List Checklist


type alias Checklist =
    { id : String
    , name : String
    }


checklistsDecoder : Decoder Checklists
checklistsDecoder =
    list checklistDecoder


checklistDecoder : Decoder Checklist
checklistDecoder =
    map2 Checklist
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

                CardChecklists cards cardChecklists ->
                    div []
                        (List.map
                            (\c ->
                                div [ class "projectCard" ]
                                    [ text c.name
                                    , br [] []
                                    , Dict.get c.id cardChecklists
                                        |> Maybe.map
                                            (\cls ->
                                                case List.length (List.filter (\cl -> cl.name == "Actions") cls) of
                                                    0 ->
                                                        "No actions list"

                                                    1 ->
                                                        "Yes"

                                                    _ ->
                                                        "Too many actions lists"
                                            )
                                        |> Maybe.withDefault "Loading...?"
                                        |> text
                                    ]
                            )
                            cards
                        )
            ]
        ]


httpErrToString : Http.Error -> String
httpErrToString error =
    case error of
        BadUrl url ->
            "The URL " ++ url ++ " was invalid"

        Timeout ->
            "Unable to reach the server, try again"

        NetworkError ->
            "Unable to reach the server, check your network connection"

        BadStatus code ->
            "Unable to get data. Status: " ++ String.fromInt code

        BadBody errorMessage ->
            "Data received was not in the correct format. Error message: " ++ errorMessage
