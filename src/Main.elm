module Main exposing (Model, Msg(..), init, main, update, view, viewInput, viewValidation)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http exposing (..)



-- MAIN


main =
    Browser.element
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
    { userType : UserType
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
    { ui : UiState
    , submittedOnce : Bool
    , gotJoke : Bool
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
    User Regular "Felix" (Just 29)


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
            ( { model | gotJoke = False }
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
                    ( { model | jokes = model.jokes ++ [ joke ], gotJoke = True }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        DeleteUser user ->
            ( model, Cmd.none )

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


userFromModel : Model -> User
userFromModel model =
    User Regular model.ui.form.name (Just model.ui.form.age)


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
-- helper for bools


boolToString : Bool -> String
boolToString bool =
    case bool of
        True ->
            "Wahr"

        False ->
            "Falsch"


view : Model -> Html Msg
view model =
    case model.users of
        [] ->
            div [] [ text "nothing to see here" ]

        list ->
            div []
                [ label [] [ text "Name" ]
                , viewInput "text" "Name" model.ui.form.name Name
                , label [] [ text "Age" ]
                , viewInput "number" "Age" (String.fromInt model.ui.form.age) Age
                , button [ onClick Submit ] [ text "Submit" ]
                , button [ onClick GetJoke ] [ text "Get Joke" ]
                , div []
                    [ h3 [] [ text "Users" ]
                    , ul []
                        (List.map viewUser model.users)
                    ]
                , div []
                    [ h3 [] [ text "Jokes" ]
                    , p [] [ text (boolToString model.gotJoke) ]
                    , ul []
                        (List.map viewJoke model.jokes)
                    ]
                , viewValidation model.submittedOnce model.ui.valid
                ]


viewJoke : String -> Html Msg
viewJoke joke =
    li [] [ text joke ]


viewUser : User -> Html Msg
viewUser user =
    li []
        [ userInfoText user
        , button [ onClick (DeleteUser user) ] [ text "Delete (TBD)" ]
        ]


userInfoText : User -> Html Msg
userInfoText user =
    text (String.join ", " [ user.name, String.fromInt (Maybe.withDefault 20 user.age) ])


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
