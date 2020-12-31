module Main exposing (..)

import Bootstrap.Progress as Progress
import Browser
import Browser.Navigation
import Dict exposing (Dict)
import Html exposing (Attribute, Html, br, button, div, span, text)
import Html.Attributes exposing (class)
import Html.Events as Events exposing (onClick)
import Http exposing (Error(..))
import Json.Decode as Decode exposing (Decoder, Value, decodeValue, errorToString, field, float, list, map2, map3, map4, maybe, string)
import List.Extra
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
    | ListState
        { listId : String
        , doneListId : Maybe String
        , state : ListState
        }


type ListState
    = GettingListCards
    | CardsGetError String
    | Items Items
    | MarkCheckItemDoneError String
    | MoveItemToDoneListError String
    | ChecklistsGetError String
    | ChecklistsUpdateError String


type alias Items =
    Dict String ( Card, NextActionsModel )


type NextActionsModel
    = NextActions NextActions
    | Loading
    | Err NextActionModelErr


type NextActionModelErr
    = NoChecklists
    | NoActionsChecklists
    | TooManyActionsChecklists (List String)


itemsFromCards : Cards -> Items
itemsFromCards =
    List.map (\card -> ( card.id, ( card, Loading ) ))
        >> Dict.fromList


updateCardChecklists : String -> Checklists -> Items -> Result String Items
updateCardChecklists cardId checklists items =
    Dict.get cardId items
        |> Result.fromMaybe ("No card exists at ID: " ++ cardId)
        |> Result.map (\( card, _ ) -> Dict.insert cardId ( card, checklistsToNextActionsResult checklists ) items)


checklistsToNextActionsResult : Checklists -> NextActionsModel
checklistsToNextActionsResult cls =
    case cls of
        [] ->
            Err NoChecklists

        _ ->
            case List.filter (\cl -> List.member cl.name [ "Checklist", "Actions", "ToDo" ]) cls of
                [] ->
                    Err NoActionsChecklists

                [ cl ] ->
                    checklistItemsToNextActions cl.checkItems
                        |> NextActions

                candidates ->
                    Err <| TooManyActionsChecklists <| List.map (\cl -> cl.name) candidates


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

                            Result.Err err ->
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

        Result.Err err ->
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
            Result.Err "No fragment segments to get token from"

        [ tokenKey, token ] ->
            if tokenKey == "token" then
                Ok token

            else
                Result.Err "First key of first fragment pair was not 'token'"

        _ ->
            Result.Err "Fragments were not in expected format"


type Msg
    = Never
    | Authorize APIConfig
    | GetLists String
    | ListsReceived (Result Http.Error Lists)
    | ListSelected String
    | ListMsg ListMsg
    | GoToProject Card
    | ReceivedUnauthorisedResponse


type ListMsg
    = CardsReceived (Result Http.Error Cards)
    | ChecklistsReceived String (Result Http.Error Checklists)
    | MarkCheckItemDone { cardId : String, checkItemId : String }
    | MarkCheckItemDoneResult String (Result Http.Error CheckItem)
    | MoveProjectToDoneList { cardId : String, doneListId : String }
    | MoveProjectToDoneListResult String (Result Http.Error Card)


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
                    Result.Err httpErr ->
                        ( ListsGetError <| "Error getting boards: " ++ httpErrToString httpErr
                        , Cmd.none
                        )

                    Ok lists ->
                        ( SelectingList lists
                        , Cmd.none
                        )

        ListSelected listId ->
            ( updateAuthRuntimeState runtime <|
                ListState
                    { listId = listId
                    , doneListId = extractDoneListId runtime.state
                    , state = GettingListCards
                    }
            , getCards runtime.credentials listId
            )

        ListMsg listMsg ->
            case runtime.state of
                ListState listState ->
                    listStateUpdate listMsg listState.listId listState.state
                        |> Tuple.mapBoth
                            (\newListState ->
                                updateAuthRuntimeState runtime <|
                                    ListState
                                        { listState
                                            | state = newListState
                                        }
                            )
                            (\f -> f runtime.credentials)

                _ ->
                    ( Error "Received listMsg at unexpected time", Cmd.none )

        GoToProject card ->
            ( Authorized runtime, Browser.Navigation.load <| "https://trello.com/c/" ++ card.id )

        ReceivedUnauthorisedResponse ->
            --TODO: should probably show that there has been an error here
            --TODO: remove stored token
            ( Unauthorized, Cmd.none )


listStateUpdate : ListMsg -> String -> ListState -> ( ListState, RequestCredentials -> Cmd Msg )
listStateUpdate listMsg listId listState =
    case listMsg of
        CardsReceived result ->
            case listState of
                GettingListCards ->
                    case result of
                        Result.Err httpErr ->
                            ( CardsGetError <| "Error getting cards: " ++ httpErrToString httpErr
                            , \_ -> Cmd.none
                            )

                        Ok cards ->
                            ( Items <| itemsFromCards cards
                            , \credentials -> Cmd.batch (List.map (\c -> getChecklists credentials c.id) cards)
                            )

                Items _ ->
                    case result of
                        Result.Err httpErr ->
                            ( CardsGetError <| "Error getting cards: " ++ httpErrToString httpErr
                            , \_ -> Cmd.none
                            )

                        Ok cards ->
                            ( Items <| itemsFromCards cards
                            , \credentials -> Cmd.batch (List.map (\c -> getChecklists credentials c.id) cards)
                            )

                _ ->
                    ( CardsGetError "Received cards at unexpected time"
                    , \_ -> Cmd.none
                    )

        ChecklistsReceived cardId result ->
            case listState of
                Items items ->
                    case result of
                        Ok newChecklists ->
                            ( updateCardChecklists cardId newChecklists items
                                |> Result.Extra.mapBoth ChecklistsUpdateError Items
                                |> Result.Extra.merge
                            , \_ -> Cmd.none
                            )

                        Result.Err error ->
                            ( ChecklistsGetError <| httpErrToString error, \_ -> Cmd.none )

                _ ->
                    ( ChecklistsGetError "Received checklists at unexpected time", \_ -> Cmd.none )

        MarkCheckItemDone { cardId, checkItemId } ->
            ( listState, \credentials -> putMarkCheckItemAsDone credentials cardId checkItemId )

        MarkCheckItemDoneResult cardId result ->
            case listState of
                Items _ ->
                    case result of
                        Ok _ ->
                            ( listState
                            , \credentials -> getChecklists credentials cardId
                            )

                        Result.Err error ->
                            ( MarkCheckItemDoneError <| httpErrToString error, \_ -> Cmd.none )

                _ ->
                    ( MarkCheckItemDoneError "MarkCheckItemDoneResult received at unexpected time", \_ -> Cmd.none )

        MoveProjectToDoneListResult _ result ->
            case result of
                Result.Err error ->
                    ( MoveItemToDoneListError <| httpErrToString error
                    , \_ -> Cmd.none
                    )

                Ok _ ->
                    ( listState, \credentials -> getCards credentials listId )

        MoveProjectToDoneList { cardId, doneListId } ->
            ( listState, \credentials -> putMoveCardToList credentials cardId doneListId )


updateAuthRuntimeState : AuthorizedRuntime -> AuthorizedRuntimeState -> Runtime
updateAuthRuntimeState authRuntime state =
    Authorized { authRuntime | state = state }


extractDoneListId : AuthorizedRuntimeState -> Maybe String
extractDoneListId runtime =
    case runtime of
        SelectingList lists ->
            List.Extra.find (\list -> "Done" == list.name) lists |> Maybe.map .id

        _ ->
            Nothing


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
        (CardsReceived >> ListMsg)
        cardsDecoder


getChecklists : RequestCredentials -> String -> Cmd Msg
getChecklists credentials cardId =
    getItems credentials
        ("/cards/" ++ cardId ++ "/checklists")
        (ChecklistsReceived cardId >> ListMsg)
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


putMarkCheckItemAsDone : RequestCredentials -> String -> String -> Cmd Msg
putMarkCheckItemAsDone credentials cardId checkItemId =
    putItems credentials
        ("/cards/" ++ cardId ++ "/checkItem/" ++ checkItemId)
        [ "state=complete" ]
        (MarkCheckItemDoneResult cardId >> ListMsg)
        checkItemDecoder


putMoveCardToList : RequestCredentials -> String -> String -> Cmd Msg
putMoveCardToList credentials cardId newListId =
    putItems credentials
        ("/cards/" ++ cardId)
        [ "state=complete", "idList=" ++ newListId, "pos=top" ]
        (MoveProjectToDoneListResult cardId >> ListMsg)
        cardDecoder


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
    , checkItems : CheckItems
    }


checklistsDecoder : Decoder Checklists
checklistsDecoder =
    list checklistDecoder


checklistDecoder : Decoder Checklist
checklistDecoder =
    map3 Checklist
        (field "id" string)
        (field "name" string)
        (field "checkItems" checkItemsDecoder)


type alias CheckItems =
    List CheckItem


type alias CheckItem =
    { pos : Float
    , name : String
    , id : String
    , state : String
    }


checkItemsDecoder : Decoder CheckItems
checkItemsDecoder =
    list checkItemDecoder


checkItemDecoder : Decoder CheckItem
checkItemDecoder =
    map4 CheckItem
        (field "pos" float)
        (field "name" string)
        (field "id" string)
        (field "state" string)


type NextActions
    = Complete
    | InProgress Actions
    | Backlogged Actions
    | EmptyList


completeNormalisedPercent : Actions -> Float
completeNormalisedPercent nextActions =
    1 - toFloat nextActions.incomplete / toFloat nextActions.total


completePercent : Actions -> Float
completePercent nextActions =
    completeNormalisedPercent nextActions * 100


type alias Actions =
    { nextAction : CheckItem
    , total : Int
    , incomplete : Int
    }


checklistItemsToNextActions : CheckItems -> NextActions
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
                        Backlogged <| Actions first itemsLength incompletesLength

                    else
                        InProgress <|
                            Actions first itemsLength incompletesLength



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
                    [ button (onClick <| Authorize model.apiConfig) [ text "Authorize" ] ]

                Authorized authorizedRuntime ->
                    viewAuthorized authorizedRuntime
        ]


onClick : Msg -> List (Attribute Msg)
onClick msg =
    [ class "clickable", Events.onClick msg ]


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
                (List.map
                    (\l ->
                        button
                            (onClick <|
                                ListSelected l.id
                            )
                            [ text l.name ]
                    )
                    lists
                )
            ]

        ListState listState ->
            case listState.state of
                GettingListCards ->
                    [ text <| "Loading projects... " ]

                MarkCheckItemDoneError err ->
                    [ text err ]

                CardsGetError err ->
                    [ text err ]

                Items items ->
                    Dict.values items
                        |> List.sortBy
                            (\( _, nextActionsResult ) -> maybeNextActionsSortValue nextActionsResult)
                        |> List.map
                            (\item -> projectCard item listState.doneListId)

                MoveItemToDoneListError err ->
                    [ text err ]

                ChecklistsGetError err ->
                    [ text err ]

                ChecklistsUpdateError err ->
                    [ text err ]


maybeNextActionsSortValue : NextActionsModel -> Float
maybeNextActionsSortValue model =
    case model of
        Err errModel ->
            case errModel of
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

        Loading ->
            8


projectCard : ( Card, NextActionsModel ) -> Maybe String -> Html Msg
projectCard ( card, res ) doneListId =
    div
        [ class "projectCard" ]
    <|
        List.append
            [ span
                (class "projectTitle" :: (onClick <| GoToProject card))
                [ text <| card.name ]
            , br [] []
            ]
            (projectCardBody res card doneListId)


projectCardBody : NextActionsModel -> Card -> Maybe String -> List (Html Msg)
projectCardBody model card maybeDoneListId =
    case model of
        Err errModel ->
            case errModel of
                NoChecklists ->
                    [ span (onClick <| GoToProject card) <| [ text "️😖 no lists" ] ]

                NoActionsChecklists ->
                    [ span (onClick <| GoToProject card) <| [ text "🧐 no actions list" ] ]

                TooManyActionsChecklists names ->
                    [ span (onClick <| GoToProject card) <| [ text <| "😕 more than one actions list: " ++ String.join ", " names ] ]

        NextActions nas ->
            case nas of
                InProgress incompleteActions ->
                    [ div [ class "cardBodyWithButtons" ] <|
                        bodyWithButtons
                            card
                            [ text <| incompleteActions.nextAction.name, actionsSmallTag incompleteActions ]
                            [ markCheckItemDoneButton card incompleteActions.nextAction (markCheckitemDoneButtonText incompleteActions) ]
                    , Progress.progress [ Progress.value <| completePercent incompleteActions ]
                    ]

                Backlogged incompleteActions ->
                    [ div [ class "cardBodyWithButtons" ] <|
                        bodyWithButtons
                            card
                            [ text <| "😌 not started: " ++ incompleteActions.nextAction.name, actionsSmallTag incompleteActions ]
                            [ markCheckItemDoneButton card incompleteActions.nextAction (markCheckitemDoneButtonText incompleteActions) ]
                    ]

                Complete ->
                    [ div [ class "cardBodyWithButtons" ] <|
                        bodyWithButtons
                            card
                            [ text "🤪 complete!" ]
                            (maybeDoneListId |> Maybe.map (\id -> [ moveProjectToDoneListButton card id ]) |> Maybe.withDefault [])
                    ]

                EmptyList ->
                    [ span (onClick <| GoToProject card) <| [ text <| "🧐 actions list has no items" ] ]

        Loading ->
            [ span (onClick <| GoToProject card) <| [ text "⏳ loading..." ] ]


actionsSmallTag : Actions -> Html Msg
actionsSmallTag actions =
    if actions.incomplete == 1 then
        smallTag "✨ last one! ✨"

    else
        smallTag <| "+" ++ (String.fromInt <| actions.incomplete - 1)


markCheckitemDoneButtonText : Actions -> String
markCheckitemDoneButtonText actions =
    if actions.incomplete == 1 then
        "Finished!"

    else if actions.incomplete == actions.total then
        "Started!"

    else
        "Next!"


bodyWithButtons : Card -> List (Html Msg) -> List (Html Msg) -> List (Html Msg)
bodyWithButtons card message buttons =
    [ div (onClick <| GoToProject card) message
    , div [] buttons
    ]


smallTag : String -> Html Msg
smallTag text =
    span [ class "smallTag" ] [ Html.text text ]


markCheckItemDoneButton : Card -> CheckItem -> String -> Html Msg
markCheckItemDoneButton card checkItem text =
    button
        (class "markCheckItemDoneButton" :: (onClick <| ListMsg <| MarkCheckItemDone { cardId = card.id, checkItemId = checkItem.id }))
        [ Html.text text ]


moveProjectToDoneListButton : Card -> String -> Html Msg
moveProjectToDoneListButton card doneListId =
    button
        (class "moveProjectToDoneListButton" :: (onClick <| ListMsg <| MoveProjectToDoneList { cardId = card.id, doneListId = doneListId }))
        [ Html.text "Archive!" ]


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
