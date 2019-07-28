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


type alias Model =
    { name : String
    , password : String
    , age : Int
    , submittedOnce : Bool
    , users : List User
    , valid : Bool
    }


init : Model
init =
    Model ""
        ""
        20
        False
        [ { userType = Regular, name = "Felix", password = "", age = Just 29 }
        , { userType = Visitor, name = "Yasna", password = "", age = Nothing }
        ]
        False



-- UPDATE


type Msg
    = Name String
    | Password String
    | Age String
    | Submit


update : Msg -> Model -> Model
update msg model =
    case msg of
        Name name ->
            { model | name = name }

        Password password ->
            { model | password = password, valid = isValid model }

        Age age ->
            { model | age = Maybe.withDefault 0 (String.toInt age) }

        Submit ->
            submitHandler model


submitHandler : Model -> Model
submitHandler model =
    case model.valid of
        True ->
            { model
                | submittedOnce = False
                , name = ""
                , password = ""
                , age = 20
                , users = model.users ++ [ userFromModel model ]
            }

        False ->
            { model | submittedOnce = True }


userFromModel : Model -> User
userFromModel model =
    User Regular model.name model.password (Just model.age)



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ label [] [ text "Name" ]
        , viewInput "text" "Name" model.name Name
        , label [] [ text "Pasword" ]
        , viewInput "password" "Password" model.password Password
        , label [] [ text "Age" ]
        , viewInput "number" "Age" (String.fromInt model.age) Age
        , button [ onClick Submit ] [ text "Submit" ]
        , viewUsers model.users
        , viewValidation model.submittedOnce model.valid
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
        coloredDiv "red" "Password needs to be at least 8 characters long!"

    else
        coloredDiv "green" "OK"


isValid : Model -> Bool
isValid model =
    if String.length model.password < 6 then
        False

    else
        True


coloredDiv : String -> String -> Html msg
coloredDiv c t =
    div [ style "color" c ] [ text t ]
