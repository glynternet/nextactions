module Main exposing (..)

import Bootstrap.Progress as Progress
import Browser
import Browser.Navigation
import Dict exposing (Dict)
import Html exposing (Html, br, button, div, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http exposing (Error(..))
import Json.Decode as Decode exposing (Decoder, Value, decodeValue, errorToString, field, float, list, map2, map3, map4, maybe, string)
import Ports
import Result.Extra
import String exposing (join)
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
    { runtimeModel : Runtime
    , apiConfig : APIConfig
    }


type alias APIConfig =
    { apiKey : String
    , loginRedirect : String
    }


type Runtime
    = Error String
    | Unauthorized
    | Authorized AuthorizedRuntime


type alias AuthorizedRuntime =
    { credentials : RequestCredentials
    , config : AuthorizedRuntimeConfig
    , state : AuthorizedRuntimeState
    }


type alias AuthorizedRuntimeConfig =
    { boardId : String
    }


type AuthorizedRuntimeState
    = GettingBoardLists
    | ListsGetError String
    | SelectingList Lists
    | GettingListCards
    | FindListError String
    | CardsGetError String
    | CardGetError String
    | Items Cards (Dict String Checklists)
    | MarkCheckitemDoneError String


type alias Config =
    { apiKey : String
    , token : Maybe String
    , boardId : String
    , loginRedirect : String
    }


configDecoder : Decoder Config
configDecoder =
    map4 Config
        (field "apiKey" string)
        (maybe <| field "token" string)
        (field "boardId" string)
        (field "loginRedirect" string)


type alias BoardID =
    String


init : Value -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init configValue url _ =
    case decodeValue configDecoder configValue of
        Ok config ->
            let
                apiConfig =
                    APIConfig config.apiKey config.loginRedirect
            in
            url.fragment
                |> Maybe.map
                    (\frag ->
                        case String.split "=" frag |> parseTokenFromFragment of
                            Ok token ->
                                let
                                    credentials =
                                        RequestCredentials config.apiKey token
                                in
                                ( Model
                                    (Authorized <| AuthorizedRuntime credentials (AuthorizedRuntimeConfig config.boardId) <| GettingBoardLists)
                                    apiConfig
                                , Cmd.batch [ storeToken token, getLists credentials config.boardId ]
                                )

                            Err err ->
                                ( Model (Error err) apiConfig
                                , Cmd.none
                                )
                    )
                |> Maybe.withDefault
                    (case config.token of
                        Just token ->
                            let
                                credentials =
                                    RequestCredentials config.apiKey token
                            in
                            ( Model
                                (Authorized <|
                                    AuthorizedRuntime
                                        credentials
                                        (AuthorizedRuntimeConfig config.boardId)
                                        GettingBoardLists
                                )
                                apiConfig
                            , getLists credentials config.boardId
                            )

                        Nothing ->
                            ( Model Unauthorized apiConfig
                            , Cmd.none
                            )
                    )

        Err err ->
            ( Model (Error <| "Error decoding the init config: " ++ errorToString err) (APIConfig "" "")
            , Cmd.none
            )


storeToken : String -> Cmd msg
storeToken token =
    Ports.storeToken token


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
    | Authorize APIConfig
    | GetLists String
    | ListsReceived (Result Http.Error Lists)
    | ListSelected String
    | CardsReceived (Result Http.Error Cards)
    | CardReceived (Result Http.Error Card)
    | ChecklistsReceived String (Result Http.Error Checklists)
    | MarkCheckitemDone String String
    | MarkCheckitemDoneResult String (Result Http.Error Checkitem)
    | GoToProject Card
    | ReceivedUnauthorisedResponse


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    runtimeUpdate msg model.runtimeModel
        |> (\( runtime, cmd ) -> ( Model runtime model.apiConfig, cmd ))


runtimeUpdate : Msg -> Runtime -> ( Runtime, Cmd Msg )
runtimeUpdate msg runtime =
    case runtime of
        Error string ->
            ( Error string, Cmd.none )

        Unauthorized ->
            case msg of
                Authorize apiConfig ->
                    ( runtime
                    , navigateToTrelloAuthorisePage apiConfig
                    )

                _ ->
                    ( runtime, Cmd.none )

        Authorized authorizedRuntime ->
            authorizedRuntimeUpdate msg authorizedRuntime


authorizedRuntimeUpdate : Msg -> AuthorizedRuntime -> ( Runtime, Cmd Msg )
authorizedRuntimeUpdate msg runtime =
    case msg of
        Never ->
            ( Authorized runtime, Cmd.none )

        Authorize apiConfig ->
            ( Authorized runtime
            , navigateToTrelloAuthorisePage apiConfig
            )

        GetLists id ->
            ( Authorized runtime, getLists runtime.credentials id )

        ListsReceived result ->
            (\( state, cmd ) -> ( updateAuthRuntimeState runtime state, cmd )) <|
                case result of
                    Err httpErr ->
                        ( ListsGetError <| "Error getting boards: " ++ httpErrToString httpErr
                        , Cmd.none
                        )

                    Ok lists ->
                        ( SelectingList lists
                        , Cmd.none
                        )

        ListSelected listId ->
            ( updateAuthRuntimeState runtime GettingListCards, getCards runtime.credentials listId )

        CardsReceived result ->
            (\( state, cmd ) -> ( updateAuthRuntimeState runtime state, cmd )) <|
                case result of
                    Err httpErr ->
                        ( CardsGetError <| "Error getting cards: " ++ httpErrToString httpErr
                        , Cmd.none
                        )

                    Ok cards ->
                        ( Items cards Dict.empty
                        , Cmd.batch (List.map (\c -> getChecklists runtime.credentials c.id) cards)
                        )

        CardReceived result ->
            (\( state, cmd ) -> ( updateAuthRuntimeState runtime state, cmd )) <|
                case runtime.state of
                    Items cards _ ->
                        case result of
                            Err httpErr ->
                                ( CardGetError <| "Error getting card: " ++ httpErrToString httpErr, Cmd.none )

                            Ok card ->
                                if List.member card cards then
                                    ( runtime.state
                                    , getChecklists runtime.credentials card.id
                                    )

                                else
                                    ( CardGetError <| "Received card is not known: " ++ card.name, Cmd.none )

                    _ ->
                        ( CardGetError "Received card at unexpected time", Cmd.none )

        ChecklistsReceived cardId result ->
            (\( state, cmd ) -> ( updateAuthRuntimeState runtime state, cmd )) <|
                case runtime.state of
                    Items cards checklists ->
                        case result of
                            Ok newChecklists ->
                                ( Items cards (Dict.insert cardId newChecklists checklists)
                                , Cmd.none
                                )

                            Err error ->
                                ( ListsGetError <| httpErrToString error, Cmd.none )

                    _ ->
                        ( ListsGetError "Received checklists at unexpected time", Cmd.none )

        MarkCheckitemDone cardId checkitemId ->
            ( Authorized runtime, putMarkCheckitemAsDone runtime.credentials cardId checkitemId )

        MarkCheckitemDoneResult cardId result ->
            (\( state, cmd ) -> ( updateAuthRuntimeState runtime state, cmd )) <|
                case runtime.state of
                    Items _ _ ->
                        case result of
                            Ok _ ->
                                ( runtime.state
                                , getCard runtime.credentials cardId
                                )

                            Err error ->
                                ( MarkCheckitemDoneError <| httpErrToString error, Cmd.none )

                    _ ->
                        ( MarkCheckitemDoneError "MarkCheckitemDoneResult received at unexpected time", Cmd.none )

        GoToProject card ->
            ( Authorized runtime, Browser.Navigation.load <| "https://trello.com/c/" ++ card.id )

        ReceivedUnauthorisedResponse ->
            --TODO: should probably show that there has been an error here
            --TODO: remove stored token
            ( Unauthorized, Cmd.none )


updateAuthRuntimeState : AuthorizedRuntime -> AuthorizedRuntimeState -> Runtime
updateAuthRuntimeState authRuntime state =
    Authorized { authRuntime | state = state }


navigateToTrelloAuthorisePage : APIConfig -> Cmd msg
navigateToTrelloAuthorisePage apiConfig =
    Browser.Navigation.load
        (apiBaseUrl
            ++ "/authorize?"
            ++ ([ ( "expiration", "1day" )
                , ( "name", "Next!" )
                , ( "scope", "read,write" )
                , ( "response_type", "token" )
                , ( "key", apiConfig.apiKey )
                , ( "return_url", apiConfig.loginRedirect )
                ]
                    |> List.map (\( k, v ) -> k ++ "=" ++ v)
                    |> join "&"
               )
        )


getLists : RequestCredentials -> String -> Cmd Msg
getLists credentials localBoardId =
    getItems credentials
        ("/boards/" ++ localBoardId ++ "/lists")
        ListsReceived
        listsDecoder


getCards : RequestCredentials -> String -> Cmd Msg
getCards credentials listId =
    getItems credentials
        ("/lists/" ++ listId ++ "/cards")
        CardsReceived
        cardsDecoder


getCard : RequestCredentials -> String -> Cmd Msg
getCard credentials cardId =
    getItems credentials
        ("/cards/" ++ cardId)
        CardReceived
        cardDecoder


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
        , expect =
            Http.expectJson
                (interceptKnownHTTPError toMsg)
                decoder
        }


interceptKnownHTTPError : (Result Error a -> Msg) -> (Result Error a -> Msg)
interceptKnownHTTPError toMsg =
    \res ->
        res
            |> Result.map (\_ -> toMsg res)
            |> Result.mapError
                (\err ->
                    if httpErrIsUnauthorised err then
                        ReceivedUnauthorisedResponse

                    else
                        toMsg res
                )
            |> Result.Extra.merge


httpErrIsUnauthorised : Http.Error -> Bool
httpErrIsUnauthorised error =
    case error of
        BadStatus code ->
            code == 401

        _ ->
            False


putMarkCheckitemAsDone : RequestCredentials -> String -> String -> Cmd Msg
putMarkCheckitemAsDone credentials cardId checkitemId =
    putItems credentials
        ("/cards/" ++ cardId ++ "/checkItem/" ++ checkitemId)
        [ "state=complete" ]
        (MarkCheckitemDoneResult cardId)
        checkitemDecoder


putItems : RequestCredentials -> String -> List String -> (Result Error a -> Msg) -> Decode.Decoder a -> Cmd Msg
putItems credentials endpoint params toMsg decoder =
    Http.request
        { method = "PUT"
        , headers = []
        , url = apiBaseUrl ++ endpoint ++ "?" ++ credentialsParams credentials ++ List.foldl (++) "" (List.map ((++) "&") params)
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg decoder
        , timeout = Nothing
        , tracker = Nothing
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
    , id : String
    , state : String
    }


checkitemsDecoder : Decoder Checkitems
checkitemsDecoder =
    list checkitemDecoder


checkitemDecoder : Decoder Checkitem
checkitemDecoder =
    map4 Checkitem
        (field "pos" float)
        (field "name" string)
        (field "id" string)
        (field "state" string)


type NextActions
    = Complete
    | InProgress InProgressActions
    | Backlogged Checkitem
    | EmptyList


completeNormalisedPercent : InProgressActions -> Float
completeNormalisedPercent nextActions =
    1 - toFloat nextActions.incomplete / toFloat nextActions.total


completePercent : InProgressActions -> Float
completePercent nextActions =
    completeNormalisedPercent nextActions * 100


type alias InProgressActions =
    { nextAction : Checkitem
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
                [] ->
                    Complete

                first :: rest ->
                    let
                        incompletesLength =
                            List.length (first :: rest)

                        itemsLength =
                            List.length items
                    in
                    if incompletesLength == itemsLength then
                        Backlogged first

                    else
                        InProgress <|
                            InProgressActions first itemsLength incompletesLength


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

                Unauthorized ->
                    [ button [ onClick <| Authorize model.apiConfig ] [ text "Authorize" ] ]

                Authorized authorizedRuntime ->
                    viewAuthorized authorizedRuntime
        ]


viewAuthorized : AuthorizedRuntime -> List (Html Msg)
viewAuthorized runtime =
    case runtime.state of
        GettingBoardLists ->
            [ text <| "Loading..." ]

        ListsGetError err ->
            [ text err ]

        SelectingList lists ->
            [ div
                [ class "listSelectionContainer" ]
                (List.map (\l -> button [ onClick <| ListSelected l.id ] [ text l.name ]) lists)
            ]

        GettingListCards ->
            [ text <| "Loading projects... " ]

        FindListError err ->
            [ text err ]

        CardsGetError err ->
            [ text err ]

        CardGetError err ->
            [ text err ]

        MarkCheckitemDoneError err ->
            [ text err ]

        Items cards cardChecklists ->
            cards
                |> List.map
                    (\c ->
                        ( c
                        , Dict.get c.id cardChecklists
                            |> Maybe.map checklistsToNextActionsResult
                        )
                    )
                |> List.sortBy
                    (\( _, maybeNas ) -> maybeNextActionsSortValue maybeNas)
                |> List.filterMap
                    (\( card, maybeNas ) ->
                        maybeNas
                            |> Maybe.map
                                (\res ->
                                    div
                                        [ class "projectCard" ]
                                    <|
                                        List.append
                                            [ span
                                                [ class "projectTitle", onClick <| GoToProject card ]
                                                [ text <| card.name ]
                                            , br [] []
                                            ]
                                            (nextActionsCard res card)
                                )
                    )


maybeNextActionsSortValue : Maybe NextActionsResult -> Float
maybeNextActionsSortValue maybeNas =
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

                        InProgress incompleteNas ->
                            6 + (1 - completeNormalisedPercent incompleteNas)

                        Backlogged _ ->
                            7


nextActionsCard : NextActionsResult -> Card -> List (Html Msg)
nextActionsCard result card =
    case result of
        NoChecklists ->
            [ span [ onClick <| GoToProject card ] <| [ text "️😖 no lists" ] ]

        NoActionsChecklists ->
            [ span [ onClick <| GoToProject card ] <| [ text "\u{1F9D0} no actions list" ] ]

        TooManyActionsChecklists names ->
            [ span [ onClick <| GoToProject card ] <| [ text <| "😕 more than one actions list: " ++ String.join ", " names ] ]

        NextActions nas ->
            case nas of
                InProgress incompleteActions ->
                    [ div [ class "cardBodyWithButtons", onClick <| GoToProject card ] <|
                        if incompleteActions.incomplete == 1 then
                            bodyWithButtons
                                [ text <| incompleteActions.nextAction.name, smallTag "✨ last one! ✨" ]
                                [ markCheckitemDoneButton card incompleteActions.nextAction "Finish!" ]

                        else
                            bodyWithButtons
                                [ text <| incompleteActions.nextAction.name
                                , smallTag <| "+" ++ (String.fromInt <| incompleteActions.incomplete - 1)
                                ]
                                [ markCheckitemDoneButton card incompleteActions.nextAction "Next!" ]
                    , Progress.progress [ Progress.value <| completePercent incompleteActions ]
                    ]

                Complete ->
                    [ span [ onClick <| GoToProject card ] <| [ text "\u{1F92A} complete!" ] ]

                EmptyList ->
                    [ span [ onClick <| GoToProject card ] <| [ text <| "\u{1F9D0} actions list has no items" ] ]

                Backlogged first ->
                    [ div [ class "cardBodyWithButtons", onClick <| GoToProject card ] <|
                        bodyWithButtons
                            [ text <| "😌 not started: " ++ first.name ]
                            [ markCheckitemDoneButton card first "Started!" ]
                    ]


bodyWithButtons : List (Html Msg) -> List (Html Msg) -> List (Html Msg)
bodyWithButtons message buttons =
    [ div [] message
    , div [] buttons
    ]


smallTag : String -> Html Msg
smallTag text =
    span [ class "smallTag" ] [ Html.text text ]


markCheckitemDoneButton : Card -> Checkitem -> String -> Html Msg
markCheckitemDoneButton card checkItem text =
    button [ onClick <| MarkCheckitemDone card.id checkItem.id, class "markCheckitemDoneButton" ] [ Html.text text ]


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
