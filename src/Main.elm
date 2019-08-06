module Main exposing (Model, Msg(..), init, main, update, view, viewInput, viewValidation)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http exposing (..)
import Random
import Random.Char
import Random.String
import Url



-- MAIN


main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type UserType
    = Regular
    | Visitor


type alias User =
    { id : String
    , userType : UserType
    , name : String
    , age : Maybe Int
    }


type alias UiFormState =
    { name : String
    , age : Int
    }


type alias UiState =
    { form : UiFormState
    , valid : Bool
    }


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , ui : UiState
    , submittedOnce : Bool
    , gettingJoke : Bool
    , users : List User
    , jokes : List String
    }


initialUiFormState : UiFormState
initialUiFormState =
    { name = "", age = 20 }


initialUiState : UiState
initialUiState =
    { form = initialUiFormState, valid = False }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        model : Model
        model =
            Model key url initialUiState False False [] []
    in
    ( model, Cmd.none )



-- UPDATE


type Msg
    = Name String
    | Age String
    | GetJoke
    | GotJoke (Result Http.Error String)
    | DeleteUser User
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | Submit


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- TODO: refactor this. Difference is only which field gets updated
        Name name ->
            let
                updatedForm : UiFormState
                updatedForm =
                    setName name model.ui.form

                oldUiState : UiState
                oldUiState =
                    model.ui

                updatedUiState : UiState
                updatedUiState =
                    { oldUiState | form = updatedForm, valid = isValid model }
            in
            ( { model | ui = updatedUiState }, Cmd.none )

        Age age ->
            let
                updatedForm : UiFormState
                updatedForm =
                    setAge age model.ui.form

                oldUiState : UiState
                oldUiState =
                    model.ui

                updatedUiState : UiState
                updatedUiState =
                    { oldUiState | form = updatedForm, valid = isValid model }
            in
            ( { model | ui = updatedUiState }, Cmd.none )

        GetJoke ->
            let
                headers : List Http.Header
                headers =
                    [ Http.header "Accept" "text/plain" ]
            in
            ( { model | gettingJoke = True }
            , Http.request
                { method = "GET"
                , headers = headers
                , url = "https://icanhazdadjoke.com/"
                , body = Http.emptyBody
                , expect = Http.expectString GotJoke
                , timeout = Nothing
                , tracker = Nothing
                }
            )

        GotJoke result ->
            case result of
                Ok joke ->
                    ( { model | jokes = model.jokes ++ [ joke ], gettingJoke = False }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        DeleteUser user ->
            let
                newUsers =
                    List.filter (\userToCheck -> userToCheck.id /= user.id) model.users
            in
            ( { model | users = newUsers }, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }, Cmd.none )

        Submit ->
            case model.ui.valid of
                True ->
                    ( { model
                        | submittedOnce = False
                        , ui = initialUiState
                        , users = model.users ++ [ userFromModel model ]
                      }
                    , Cmd.none
                    )

                False ->
                    ( { model | submittedOnce = True }, Cmd.none )


setName : String -> UiFormState -> UiFormState
setName newName form =
    { form | name = newName }


setAge : String -> UiFormState -> UiFormState
setAge newAge form =
    case String.toInt newAge of
        Nothing ->
            form

        Just age ->
            { form | age = age }


seed : Random.Seed
seed =
    Random.initialSeed 42


randomUserId : String
randomUserId =
    let
        ( id, nextSeed ) =
            Random.step (Random.String.string 32 Random.Char.latin) seed
    in
    id


userFromModel : Model -> User
userFromModel model =
    User (randomUserId ++ model.ui.form.name) Regular model.ui.form.name (Just model.ui.form.age)


isValid : Model -> Bool
isValid model =
    if String.length model.ui.form.name < 3 then
        False

    else
        True



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = title model
    , body =
        [ text "The current URL is: "
        , b [] [ text (Url.toString model.url) ]
        , ul []
            [ viewLink "/" (Just "Home") False
            , viewLink "/profile" Nothing False
            , viewLink "/reviews/the-century-of-the-self" Nothing False
            , viewLink "/reviews/public-opinion" Nothing False
            , viewLink "https://heise.de" (Just "Heise") True
            ]
        , div [ class "grid" ]
            [ div [ class "grid__container" ]
                [ h3 [] [ text "Users" ]
                , div []
                    [ addUserForm model.ui.form
                    , viewValidation model.submittedOnce model.ui.valid
                    , ul []
                        (List.map viewUser model.users)
                    ]
                ]
            , div [ class "grid__container" ]
                [ h3 [] [ text "Jokes" ]
                , div []
                    [ getJokeButton model.gettingJoke
                    , ul []
                        (List.map viewJoke model.jokes)
                    ]
                ]
            ]
        ]
    }


viewLink : String -> Maybe String -> Bool -> Html msg
viewLink path mLabel external =
    let
        basicLinkAttrs : List (Html.Attribute msg)
        basicLinkAttrs =
            [ href path
            ]

        linkAttrs : List (Html.Attribute msg)
        linkAttrs =
            case external of
                True ->
                    target "_blank" :: basicLinkAttrs

                False ->
                    basicLinkAttrs

        linkLabel : String
        linkLabel =
            case mLabel of
                Just label ->
                    label

                Nothing ->
                    path
    in
    li [ style "display" "inline", style "padding-right" "10px" ] [ a linkAttrs [ text linkLabel ] ]


viewJoke : String -> Html Msg
viewJoke joke =
    li [] [ text joke ]


viewUser : User -> Html Msg
viewUser user =
    li []
        [ userInfoText user
        , button [ onClick (DeleteUser user) ] [ text "Delete" ]
        ]


title : Model -> String
title model =
    let
        userCount =
            List.length model.users

        jokeCount =
            List.length model.jokes
    in
    "Users (" ++ String.fromInt userCount ++ ") and Jokes (" ++ String.fromInt jokeCount ++ ")"


boolToString : Bool -> String
boolToString bool =
    case bool of
        True ->
            ""

        False ->
            "Loading..."


getJokeButton : Bool -> Html Msg
getJokeButton loading =
    case loading of
        True ->
            button [ disabled True ] [ text "Loading..." ]

        False ->
            button [ onClick GetJoke ] [ text "Get Joke" ]


userInfoText : User -> Html Msg
userInfoText user =
    span [ Html.Attributes.title user.id ] [ text (String.join ", " [ user.name, String.fromInt (Maybe.withDefault 20 user.age) ]) ]


addUserForm : UiFormState -> Html Msg
addUserForm form =
    div []
        [ label [] [ text "Name" ]
        , viewInput "text" "Name" form.name Name
        , label [] [ text "Age" ]
        , viewInput "number" "Age" (String.fromInt form.age) Age
        , button [ onClick Submit ] [ text "Add user" ]
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewValidation : Bool -> Bool -> Html msg
viewValidation submitted valid =
    if not submitted then
        div [] []

    else if not valid then
        coloredDiv "red" "Name needs to be at least 3 characters long!"

    else
        coloredDiv "green" "OK"


coloredDiv : String -> String -> Html msg
coloredDiv c t =
    div [ style "color" c ] [ text t ]
