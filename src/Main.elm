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
    | Items Cards (Dict String Checklists)


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
    | ChecklistsReceived String (Result Http.Error Checklists)


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
                    ( Model model.config <| Items cards Dict.empty
                    , Cmd.batch (List.map (\c -> getChecklists credentials c.id) cards)
                    )

        ChecklistsReceived cardId result ->
            case model.runtimeModel of
                Items cards checklists ->
                    case Debug.log ("Received: " ++ cardId) result of
                        Ok newChecklists ->
                            ( Model model.config <| Items cards (Dict.insert cardId newChecklists checklists)
                            , Cmd.none
                            )

                        Err error ->
                            ( Model model.config <| Error <| httpErrToString error, Cmd.none )

                _ ->
                    ( Model model.config <| Error "Received checklists whilst in unexpected state", Cmd.none )


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


credentialsParams : RequestCredentials -> String
credentialsParams credentials =
    "key=" ++ credentials.key ++ "&token=" ++ credentials.token


getItems : RequestCredentials -> String -> (Result Error a -> Msg) -> Decode.Decoder a -> Cmd Msg
getItems credentials endpoint toMsg decoder =
    Http.get
        { url = apiBaseUrl ++ endpoint ++ "?" ++ credentialsParams credentials
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
    , checkItems : Checkitems
    }


checklistsDecoder : Decoder Checklists
checklistsDecoder =
    list checklistDecoder


checklistDecoder : Decoder Checklist
checklistDecoder =
    map3 Checklist
        (field "id" string)
        (field "name" string)
        (field "checkItems" checkitemsDecoder)


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
    | Incomplete IncompleteNextActions
    | EmptyList


percentComplete : IncompleteNextActions -> Float
percentComplete nextActions =
    1 - toFloat nextActions.incomplete / toFloat nextActions.total


type alias IncompleteNextActions =
    { nextAction : String
    , total : Int
    , incomplete : Int
    }


checklistItemsToNextActions : Checkitems -> NextActions
checklistItemsToNextActions items =
    case items of
        [] ->
            EmptyList

        _ ->
            case List.sortBy (\cli -> cli.pos) <| List.filter (\cli -> cli.state == "incomplete") items of
                first :: rest ->
                    Incomplete <| IncompleteNextActions first.name (List.length items) (List.length <| first :: rest)

                _ ->
                    Complete


type NextActionsResult
    = NoChecklists
    | NoActionsChecklists
    | TooManyActionsChecklists (List String)
    | NextActions NextActions


checklistsToNextActionsResult : Checklists -> NextActionsResult
checklistsToNextActionsResult cls =
    case cls of
        [] ->
            NoChecklists

        _ ->
            case List.filter (\cl -> List.member cl.name [ "Checklist", "Actions", "ToDo" ]) cls of
                [] ->
                    NoActionsChecklists

                [ cl ] ->
                    checklistItemsToNextActions cl.checkItems
                        |> NextActions

                candidates ->
                    TooManyActionsChecklists <| List.map (\cl -> cl.name) candidates



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

                Items cards cardChecklists ->
                    cards
                        |> List.map
                            (\c ->
                                ( c.name
                                , Dict.get c.id cardChecklists
                                    |> Maybe.map checklistsToNextActionsResult
                                )
                            )
                        |> List.sortBy
                            (\( _, maybeNas ) ->
                                case maybeNas of
                                    Nothing ->
                                        0

                                    Just nasRes ->
                                        case nasRes of
                                            NoActionsChecklists ->
                                                1

                                            TooManyActionsChecklists _ ->
                                                2

                                            NoChecklists ->
                                                3

                                            NextActions nas ->
                                                case nas of
                                                    EmptyList ->
                                                        4

                                                    Complete ->
                                                        5

                                                    Incomplete incompleteNas ->
                                                        6 + (1 - percentComplete incompleteNas)
                            )
                        |> List.map
                            (\( name, maybeNas ) ->
                                div [ class "projectCard" ]
                                    [ text name
                                    , br [] []
                                    , maybeNas
                                        |> Maybe.map
                                            (\res ->
                                                case res of
                                                    NoChecklists ->
                                                        text "️😖 card contains no action lists"

                                                    NoActionsChecklists ->
                                                        text "\u{1F9D0} no actions list for project"

                                                    TooManyActionsChecklists names ->
                                                        text <| "😕 more than one actions list checklist: " ++ String.join ", " names

                                                    NextActions nas ->
                                                        case nas of
                                                            Incomplete incompleteActions ->
                                                                span [] <|
                                                                    [ text <| incompleteActions.nextAction
                                                                    , span [ class "smallTag" ]
                                                                        [ text <|
                                                                            if incompleteActions.incomplete == 1 then
                                                                                "✨ last one! ✨"

                                                                            else
                                                                                "+" ++ (String.fromInt <| incompleteActions.incomplete - 1)
                                                                        ]
                                                                    ]

                                                            Complete ->
                                                                text "\u{1F92A} you are complete"

                                                            EmptyList ->
                                                                text <| "\u{1F9D0} actions list contains no items"
                                            )
                                        |> Maybe.withDefault (text "Loading...?")
                                    ]
                            )
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
