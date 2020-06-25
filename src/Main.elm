module Main exposing (..)

import Browser
import Browser.Navigation
import Dict exposing (Dict)
import Html exposing (Html, br, button, div, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http exposing (Error(..))
import Json.Decode as Decode exposing (Decoder, Value, decodeValue, errorToString, field, float, list, map2, map3, map4, string)
import Url exposing (Protocol(..))


apiBaseUrl =
    "https://trello.com/1"



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


type alias Model =
    { config : Config
    , runtimeModel : Runtime
    }


type Runtime
    = Unauthorized APIKey String
    | Error String
    | GettingBoardLists
    | ListsGetError String
    | GettingListCards String
    | FindListError String
    | CardsGetError String
    | Items Cards (Dict String Checklists) (Dict String Checkitems)


type alias Config =
    { apiKey : String
    , boardId : String
    , listName : String
    , loginRedirect : String
    }


configDecoder : Decoder Config
configDecoder =
    map4 Config
        (field "apiKey" string)
        (field "boardId" string)
        (field "listName" string)
        (field "loginRedirect" string)


type alias BoardID =
    String


type alias APIKey =
    String


init : Value -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init configValue url _ =
    case decodeValue configDecoder configValue of
        Ok config ->
            url.fragment
                |> Maybe.map
                    (\frag ->
                        case String.split "=" frag |> parseTokenFromFragment of
                            Ok token ->
                                ( Model config GettingBoardLists
                                , getLists (RequestCredentials config.apiKey token) config.boardId
                                )

                            Err err ->
                                ( Model config <| Error err
                                , Cmd.none
                                )
                    )
                |> Maybe.withDefault
                    ( Model config <| Unauthorized config.apiKey config.loginRedirect
                    , Cmd.none
                    )

        Err err ->
            ( Model (Config "" "" "" "") (Error <| "Error decoding the init config: " ++ errorToString err)
            , Cmd.none
            )


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
    | Authorize APIKey String
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

        Authorize apiKey redirectURL ->
            ( model
            , Browser.Navigation.load
                (apiBaseUrl ++ "/authorize?expiration=1day&name=testing-login&scope=read&response_type=token&key=" ++ apiKey ++ "&return_url=" ++ redirectURL)
            )

        GetLists credentials id ->
            ( model, getLists credentials id )

        ListsReceived credentials result ->
            case result of
                Err httpErr ->
                    ( Model model.config <| ListsGetError <| "Error getting boards: " ++ httpErrToString httpErr, Cmd.none )

                Ok lists ->
                    let
                        projectLists =
                            List.filter (\l -> l.name == model.config.listName) lists
                    in
                    case projectLists of
                        [] ->
                            ( Model model.config <| FindListError "No list found with the project name", Cmd.none )

                        [ list ] ->
                            ( Model model.config <| GettingListCards list.id, getCards credentials list.id )

                        _ ->
                            ( Model model.config <| FindListError "Too many lists found with the project name", Cmd.none )

        CardsReceived credentials result ->
            case result of
                Err httpErr ->
                    ( Model model.config <| CardsGetError <| "Error getting cards: " ++ httpErrToString httpErr, Cmd.none )

                Ok cards ->
                    ( Model model.config <| Items cards Dict.empty Dict.empty
                    , Cmd.batch (List.map (\c -> getChecklists credentials c.id) cards)
                    )

        ChecklistsReceived credentials cardId result ->
            case model.runtimeModel of
                Items cards checklists checkitems ->
                    case Debug.log ("Received: " ++ cardId) result of
                        Ok newChecklists ->
                            ( Model model.config <| Items cards (Dict.insert cardId newChecklists checklists) checkitems
                            , Cmd.batch (List.map (\cl -> getCheckitems credentials cl.id) newChecklists)
                            )

                        Err error ->
                            ( Model model.config <| Error <| httpErrToString error, Cmd.none )

                _ ->
                    ( Model model.config <| Error "Received checklists whilst in unexpected state", Cmd.none )

        CheckitemsReceived checklistId result ->
            case model.runtimeModel of
                Items cards checklists checkitems ->
                    case Debug.log ("Received: " ++ checklistId) result of
                        Ok newCheckitems ->
                            ( Model model.config <| Items cards checklists (Dict.insert checklistId newCheckitems checkitems)
                            , Cmd.none
                            )

                        Err error ->
                            ( Model model.config <| Error <| "Error getting checkitems for checklist with ID:" ++ checklistId ++ " " ++ httpErrToString error, Cmd.none )

                _ ->
                    ( Model model.config <| Error "Received checkitems whilst in unexpected state", Cmd.none )


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


type NextActions
    = Complete
    | Incomplete String Int
    | EmptyList


checklistItermsToNextActions : Checkitems -> NextActions
checklistItermsToNextActions items =
    case items of
        [] ->
            EmptyList

        _ ->
            case List.sortBy (\cli -> cli.pos) <| List.filter (\cli -> cli.state == "incomplete") items of
                first :: rest ->
                    Incomplete first.name <| List.length rest

                _ ->
                    Complete



-- VIEW


view : Model -> Browser.Document Msg
view model =
    Browser.Document "Next Actions"
        [ div
            []
          <|
            case model.runtimeModel of
                Error err ->
                    [ text err ]

                Unauthorized apiKey redirectURL ->
                    [ button [ onClick <| Authorize apiKey redirectURL ] [ text "Authorize" ] ]

                GettingBoardLists ->
                    [ text <| "Loading..." ]

                ListsGetError err ->
                    [ text err ]

                GettingListCards listId ->
                    [ text <| "Loading list... " ++ listId ]

                FindListError err ->
                    [ text err ]

                CardsGetError err ->
                    [ text err ]

                Items cards cardChecklists checklistitems ->
                    List.map
                        (\c ->
                            div [ class "projectCard" ]
                                [ text c.name
                                , br [] []
                                , Dict.get c.id cardChecklists
                                    |> Maybe.map
                                        (\cls ->
                                            case cls of
                                                [] ->
                                                    text "⚠️ Card contains no action lists"

                                                [ cl ] ->
                                                    if List.member cl.name [ "Checklist", "Actions", "ToDo" ] then
                                                        Dict.get cl.id checklistitems
                                                            |> Maybe.map checklistItermsToNextActions
                                                            |> Maybe.map
                                                                (\nas ->
                                                                    case nas of
                                                                        Incomplete name incompleteCount ->
                                                                            span []
                                                                                [ text <| name
                                                                                , span [ class "smallTag" ] [ text ("+" ++ (String.fromInt <| incompleteCount)) ]
                                                                                ]

                                                                        Complete ->
                                                                            text "\u{1F92A} You are complete"

                                                                        EmptyList ->
                                                                            text <| "\u{1F9D0} Actions list contains no items"
                                                                                                                                            )
                                                            |> Maybe.withDefault
                                                                (text <| "\u{1F9D0} No checkitems found for checklist with id: " ++ cl.id)

                                                    else
                                                        text "😕 Single checklist title was not one of the keyword ones"

                                                _ ->
                                                    text "More than 1 checklist, need to filter for Actions"
                                        )
                                    |> Maybe.withDefault (text "Loading...?")
                                ]
                        )
                        cards
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
