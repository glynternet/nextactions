module Main exposing (..)

import Browser
import Browser.Navigation
import Dict exposing (Dict)
import Html exposing (Html, br, button, div, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http exposing (Error(..))
import Json.Decode as Decode exposing (Decoder, field, float, list, map2, map3, string)
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
    | Error String
    | GettingBoardLists
    | ListsGetError String
    | GettingListCards String
    | FindListError String
    | CardsGetError String
    | Items Cards (Dict String Checklists) (Dict String Checkitems)


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init _ url _ =
    url.fragment
        |> Maybe.map
            (\frag ->
                case String.split "=" frag |> parseTokenFromFragment of
                    Ok token ->
                        ( GettingBoardLists, getLists (RequestCredentials apiKey token) boardId )

                    Err err ->
                        ( Error err, Cmd.none )
            )
        |> Maybe.withDefault ( Unauthorized, Cmd.none )


type alias FragmentError =
    String


parseTokenFromFragment : List String -> Result FragmentError String
parseTokenFromFragment segments =
    case segments of
        [] ->
            Err "No fragment segments to get token from"

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
    | ChecklistsReceived RequestCredentials String (Result Http.Error Checklists)
    | CheckitemsReceived String (Result Http.Error Checkitems)


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
                    ( ListsGetError <| "Error getting boards: " ++ httpErrToString httpErr, Cmd.none )

                Ok lists ->
                    let
                        projectLists =
                            List.filter (\l -> l.name == listName) lists
                    in
                    case projectLists of
                        [] ->
                            ( FindListError "No list found with the project name", Cmd.none )

                        [ list ] ->
                            ( GettingListCards list.id, getCards credentials list.id )

                        _ ->
                            ( FindListError "Too many lists found with the project name", Cmd.none )

        CardsReceived credentials result ->
            case result of
                Err httpErr ->
                    ( CardsGetError <| "Error getting cards: " ++ httpErrToString httpErr, Cmd.none )

                Ok cards ->
                    ( Items cards Dict.empty Dict.empty
                    , Cmd.batch (List.map (\c -> getChecklists credentials c.id) cards)
                    )

        ChecklistsReceived credentials cardId result ->
            case model of
                Items cards checklists checkitems ->
                    case Debug.log ("Received: " ++ cardId) result of
                        Ok newChecklists ->
                            ( Items cards (Dict.insert cardId newChecklists checklists) checkitems
                            , Cmd.batch (List.map (\cl -> getCheckitems credentials cl.id) newChecklists)
                            )

                        Err error ->
                            ( Error <| httpErrToString error, Cmd.none )

                _ ->
                    ( Error "Received checklists whilst in unexpected state", Cmd.none )

        CheckitemsReceived checklistId result ->
            case model of
                Items cards checklists checkitems ->
                    case Debug.log ("Received: " ++ checklistId) result of
                        Ok newCheckitems ->
                            ( Items cards checklists (Dict.insert checklistId newCheckitems checkitems)
                            , Cmd.none
                            )

                        Err error ->
                            ( Error <| "Error getting checkitems for checklist with ID:" ++ checklistId ++ " " ++ httpErrToString error, Cmd.none )

                _ ->
                    ( Error "Received checkitems whilst in unexpected state", Cmd.none )


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
        (ChecklistsReceived credentials cardId)
        checklistsDecoder


getCheckitems : RequestCredentials -> String -> Cmd Msg
getCheckitems credentials checklistId =
    getItems credentials
        ("/checklists/" ++ checklistId ++ "/checkItems")
        (CheckitemsReceived checklistId)
        checkitemsDecoder


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


type alias Checkitems =
    List Checkitem


type alias Checkitem =
    { pos : Float
    , name : String
    , state : String
    }


checkitemsDecoder : Decoder Checkitems
checkitemsDecoder =
    list checkitemDecoder


checkitemDecoder : Decoder Checkitem
checkitemDecoder =
    map3 Checkitem
        (field "pos" float)
        (field "name" string)
        (field "state" string)



-- VIEW


view : Model -> Browser.Document Msg
view model =
    Browser.Document "Next Actions"
        [ div
            []
            [ case model of
                Error err ->
                    div [] [ text err ]

                Unauthorized ->
                    button [ onClick Authorize ] [ text "Authorize" ]

                GettingBoardLists ->
                    div [] [ text <| "Loading..." ]

                ListsGetError err ->
                    div [] [ text err ]

                GettingListCards listId ->
                    div [] [ text <| "Loading list... " ++ listId ]

                FindListError err ->
                    div [] [ text err ]

                CardsGetError err ->
                    div [] [ text err ]

                Items cards cardChecklists checklistitems ->
                    div []
                        (List.map
                            (\c ->
                                div [ class "projectCard" ]
                                    [ text c.name
                                    , br [] []
                                    , Dict.get c.id cardChecklists
                                        |> Maybe.map
                                            (\cls ->
                                                -- if single checklist called Checklist or if Actions
                                                case cls of
                                                    [] ->
                                                        text "Card contains no checklists"

                                                    --case List.length (List.filter (\cl -> cl.name == "Actions") cls) of
                                                    [ cl ] ->
                                                        if List.member cl.name [ "Checklist", "Actions", "ToDo" ] then
                                                            Dict.get cl.id checklistitems
                                                                |> Maybe.map
                                                                    (\clis ->
                                                                        case List.sortBy (\cli -> cli.pos) <| List.filter (\cli -> cli.state == "incomplete") clis of
                                                                            first :: rest ->
                                                                                span []
                                                                                    [ text <| first.name
                                                                                    , span [ class "smallTag" ] [ text ("+" ++ (String.fromInt <| List.length rest)) ]
                                                                                    ]

                                                                            _ ->
                                                                                text "No incomplete items"
                                                                    )
                                                                |> Maybe.withDefault
                                                                    (text <| "No checkitems found for checklist with id: " ++ cl.id)

                                                        else
                                                            text "Single checklist title was not one of the keyword ones"

                                                    _ ->
                                                        text "More than 1 checklist, need to filter for Actions"
                                            )
                                        |> Maybe.withDefault (text "Loading...?")
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
