module Main exposing (Model, Msg(..), init, main, update, view, viewInput, viewValidation)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type UserType
    = Regular
    | Visitor


type alias User =
    { userType : UserType
    , name : String
    , password : String
    , age : Maybe Int
    }


type alias UiFormState =
    { name : String
    , password : String
    , age : Int
    }


type alias UiState =
    { form : UiFormState
    , valid : Bool
    }


type alias Model =
    { ui : UiState
    , submittedOnce : Bool
    , users : List User
    }


emptyUiFormState : UiFormState
emptyUiFormState =
    { name = "", password = "", age = 20 }


emptyUiState : UiState
emptyUiState =
    { form = emptyUiFormState, valid = False }


defaultUsers : List User
defaultUsers =
    [ { userType = Regular, name = "Felix", password = "", age = Just 29 }
    , { userType = Visitor, name = "Yasna", password = "", age = Nothing }
    ]


init : Model
init =
    { ui = emptyUiState, submittedOnce = False, users = defaultUsers }



-- UPDATE


type Msg
    = Name String
    | Password String
    | Age String
    | Submit


update : Msg -> Model -> Model
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
            { model | ui = updatedUiState }

        Password password ->
            let
                updatedForm : UiFormState
                updatedForm =
                    setPassword password model.ui.form

                oldUiState : UiState
                oldUiState =
                    model.ui

                updatedUiState : UiState
                updatedUiState =
                    { oldUiState | form = updatedForm, valid = isValid model }
            in
            { model | ui = updatedUiState }

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
            { model | ui = updatedUiState }

        Submit ->
            submitHandler model


setName : String -> UiFormState -> UiFormState
setName newName form =
    { form | name = newName }


setPassword : String -> UiFormState -> UiFormState
setPassword newPassword form =
    { form | password = newPassword }


setAge : String -> UiFormState -> UiFormState
setAge newAge form =
    case String.toInt newAge of
        Nothing ->
            form

        Just age ->
            { form | age = age }


updateUiState : Model -> UiState
updateUiState model =
    let
        uiState : UiState
        uiState =
            model.ui

        oldForm : UiFormState
        oldForm =
            uiState.form

        updatedForm : UiFormState
        updatedForm =
            { oldForm
                | name = oldForm.name
                , password = oldForm.password
                , age = oldForm.age
            }
    in
    { uiState | form = oldForm }


submitHandler : Model -> Model
submitHandler model =
    case model.ui.valid of
        True ->
            { model
                | submittedOnce = False
                , ui = emptyUiState
                , users = model.users ++ [ userFromModel model ]
            }

        False ->
            { model | submittedOnce = True }


userFromModel : Model -> User
userFromModel model =
    User Regular model.ui.form.name model.ui.form.password (Just model.ui.form.age)


isValid : Model -> Bool
isValid model =
    if String.length model.ui.form.password < 6 then
        False

    else
        True



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ label [] [ text "Name" ]
        , viewInput "text" "Name" model.ui.form.name Name
        , label [] [ text "Pasword" ]
        , viewInput "password" "Password" model.ui.form.password Password
        , label [] [ text "Age" ]
        , viewInput "number" "Age" (String.fromInt model.ui.form.age) Age
        , button [ onClick Submit ] [ text "Submit" ]
        , viewUsers model.users
        , viewValidation model.submittedOnce model.ui.valid
        ]


viewUsers : List User -> Html Msg
viewUsers users =
    ul []
        (List.map viewUser users)


viewUser : User -> Html Msg
viewUser user =
    li [] [ userInfoText user ]


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
        coloredDiv "red" "Password needs to be at least 6 characters long!"

    else
        coloredDiv "green" "OK"


coloredDiv : String -> String -> Html msg
coloredDiv c t =
    div [ style "color" c ] [ text t ]
