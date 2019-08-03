module Main exposing (Model, Msg(..), init, main, update, view, viewInput, viewValidation)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http exposing (..)
import Random
import Random.String
import Random.Char


-- MAIN


main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
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


type alias Document msg =
    { title : String
    , body : List (Html msg)
    }


type alias Model =
    { ui : UiState
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


defaultUser : User
defaultUser =
    User "abc" Regular "Felix" (Just 29)


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model : Model
        model =
            Model initialUiState False False [ defaultUser ] []
    in
    ( model, Cmd.none )



-- UPDATE


type Msg
    = Name String
    | Age String
    | GetJoke
    | GotJoke (Result Http.Error String)
    | DeleteUser User
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
                headers : List Header
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
                newUsers = List.filter (\userToCheck -> userToCheck.id /= user.id) model.users
            in
                ( { model | users = newUsers }, Cmd.none )

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
seed = Random.initialSeed 42


randomUserId : String
randomUserId =
    let
        (id, nextSeed) = Random.step (Random.String.string 32 Random.Char.latin) seed
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


view : Model -> Document Msg
view model =
    { title = title model
    , body =
        [ div [ class "grid" ]
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


viewJoke : String -> Html Msg
viewJoke joke =
    li [] [ text joke ]


viewUser : User -> Html Msg
viewUser user =
    li []
        [ userInfoText user
        , button [ onClick (DeleteUser user) ] [ text "Delete (TBD)" ]
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
