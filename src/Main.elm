module Main exposing (main)

{-| TODO

    - [x] Implement a visually pleasing design without sacrificing usability
    - [x] Make it responsive and useable on all screen sizes
    - [/] Implement the hero in a resposive fashion
            - Feel free to adjust the HTML DOM as needed
            - The image should always take the full width of the screen (or a container)
            - The image should always be 400px in height
            - The person in the photo should always be visible regardless of screen width
            - The title and text should be vertically and horizontally aligned center and never overflow the container
    - [ ] Fix Accessability
    - [ ] Validate that password contains at least 1 capital, 1 digit and 1 special character.
    - [ ] confirmPassword and password fields must match. If not, form should not be submittable.
    - [ ] Make a show/hide button to display password in cleartext at will.
    - [x] Form should not submit unless all fields are valid.
    - [x] Form should not submit unless terms and condition checkbox is "checked"
    - [ ] Add feedback that the app is loading when submit is clicked.
    - [x] Refactor code to make it more maintainable and easier to understand.

-}

import Browser
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as Events
import Process
import Task


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type Msg
    = FieldGotInput Field
    | FormSubmitClicked
    | GotBackendResponse


type alias Model =
    { form : Form
    , view : View
    }


type View
    = FillForm
    | SendingForm
    | SubmitSuccess


type alias Flags =
    ()


type alias Form =
    { firstname : String
    , lastname : String
    , email : String
    , password : String
    , confirmPassword : String
    , agreeTerms : Bool
    }


type Field
    = Firstname String
    | Lastname String
    | Email String
    | Password String
    | ConfirmPassword String
    | AgreeTerms Bool


form_Empty : Form
form_Empty =
    { firstname = ""
    , lastname = ""
    , email = ""
    , password = ""
    , confirmPassword = ""
    , agreeTerms = False
    }


form_Update : Field -> Form -> Form
form_Update field form =
    case field of
        Firstname str ->
            { form | firstname = str }

        Lastname str ->
            { form | lastname = str }

        Email str ->
            { form | email = str }

        Password str ->
            { form | password = str }

        ConfirmPassword str ->
            { form | confirmPassword = str }
            
        AgreeTerms bool ->
            { form | agreeTerms = bool }


initModel : Model
initModel =
    { form = form_Empty
    , view = FillForm
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( initModel
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FieldGotInput field ->
            ( { model | form = form_Update field model.form }
            , Cmd.none
            )

        FormSubmitClicked ->
            ( { model | view = SendingForm }
            , Process.sleep 1000
                |> Task.andThen (\_ -> Task.succeed GotBackendResponse)
                |> Task.perform identity
            )

        GotBackendResponse ->
            ( { model | view = SubmitSuccess }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    Html.div [ HA.class "container" ]

            -- [ Html.node "style" [] [ Html.text css ]
            [view_Hero
            , case model.view of
                FillForm ->
                    view_Form model.form

                SendingForm ->
                    view_Form model.form

                SubmitSuccess ->
                    view_Success model
            ]
        


view_Hero : Html msg
view_Hero =
    Html.div
        [ HA.class "hero"
        ]
        [ Html.div
            [ HA.class "hero-text"
            ]
            [ Html.h1 [] [ Html.text "Create account!" ]
            , Html.p [] [ Html.text "Dolor eveniet mollitia omnis sequi obcaecati. Nobis sit nam iure sit earum. Dolorem natus dolore perspiciatis accusamus numquam maiores lorem!" ]
            ]
        ]


view_LabeledInput :
    { label : String
    , value : String
    , onInput : String -> msg
    , inputType : String
    , maybeError : Maybe String
    }
    -> Html msg
view_LabeledInput { label, value, onInput, inputType, maybeError } =
    Html.div [ HA.class "labeled-input" ]
        [ Html.text label
        , Html.input
            [ HA.value value
            , Events.onInput onInput
            , HA.type_ inputType
            ]
            []
        , case maybeError of
            Just errorMsg ->
                Html.div [ HA.class "error-text" ] [ Html.text errorMsg ]

            Nothing ->
                Html.text ""
        ]


view_Form : Form -> Html Msg
view_Form form =
    Html.div []
        [ Html.div [ HA.class "form" ]
            [ Html.div [ HA.class "form-fields" ]
                [ view_LabeledInput
                    { label = "Firstname"
                    , value = form.firstname
                    , onInput = FieldGotInput << Firstname
                    , inputType = "text"
                    , maybeError = Nothing
                    }
                , view_LabeledInput
                    { label = "Lastname"
                    , value = form.lastname
                    , onInput = FieldGotInput << Lastname
                    , inputType = "text"
                    , maybeError = Nothing
                    }
                , view_LabeledInput
                    { label = "Email"
                    , value = form.email
                    , onInput = FieldGotInput << Email
                    , inputType = "text"
                    , maybeError = getEmailError form.email
                    }
                , view_LabeledInput
                    { label = "Password"
                    , value = form.password
                    , onInput = FieldGotInput << Password
                    , inputType = "password"
                    , maybeError = getPasswordError form.password
                    }
                , view_LabeledInput
                    { label = "Confirm Password"
                    , value = form.confirmPassword
                    , onInput = FieldGotInput << ConfirmPassword
                    , inputType = "password"
                    , maybeError = Nothing
                    }
                ]
            , Html.div
                [ HA.class "checkbox" ]
                [ Html.input
                    [ HA.checked form.agreeTerms
                    , Events.onCheck (FieldGotInput << AgreeTerms)
                    , HA.type_ "checkbox"
                    ]
                    []
                , Html.text "I agree to terms and conditions"
                ]
            , Html.div
                []
                [ Html.button
                    [ Events.onClick FormSubmitClicked
                    , HA.disabled (not (isFormValid form))
                    ]
                    [ Html.text "Create Account"
                    ]
                ]
            ]
        , Html.div
            [ HA.class "unique-selling-points"
            ]
            [ Html.h2 [] [ Html.text "Lots of features" ]
            , Html.p [] [ Html.text "Get access to our full set of features by registering, including but not limited to:" ]
            , Html.ul
                []
                [ Html.li [] [ Html.text "Lorem ipsum" ]
                , Html.li [] [ Html.text "Dolor eveniet" ]
                , Html.li [] [ Html.text "Mollitia omnis sequi obcaecati" ]
                , Html.li [] [ Html.text "Nobis" ]
                , Html.li [] [ Html.text "Nam iure sit earum" ]
                , Html.li [] [ Html.text "Perspiciatis accusamus numquam" ]
                , Html.li [] [ Html.text "Obcaecati" ]
                , Html.li [] [ Html.text "Dolor omnis" ]
                ]
            ]
        ]


view_Success : Model -> Html msg
view_Success model =
    Html.div
        []
        [ Html.text "Thank you for registering!"
        ]


getEmailError : String -> Maybe String
getEmailError email =
    if email == "" then
        Nothing

    else if not <| String.contains "@" email then
        Just "Invalid email"

    else if String.endsWith "example.com" email then
        Just "Email already registered"

    else
        Nothing


getPasswordError : String -> Maybe String
getPasswordError password =
    if password == "" then
        Nothing

    else if String.length password < 6 then
        Just "Password must be at least 6 chars"

    else
        Nothing


isFormValid : Form -> Bool
isFormValid form =
    form.firstname
        /= ""
        && form.lastname
        /= ""
        && getEmailError form.email
        == Nothing
        && getPasswordError form.password
        == Nothing
        && form.password
        == form.confirmPassword
        && form.agreeTerms == True
