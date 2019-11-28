module Main exposing (..)

import Array exposing (Array)
import Bitwise
import Browser
import Debug
import Element exposing (Attribute, Element, alignTop, centerY, column, el, fill, padding, px, rgb255, row, shrink, spacing, table, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Json.Encode as Encode
import Maybe.Extra
import Parser exposing ((|.), (|=), Parser)



-- import Html.Events exposing (onClick)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- Model


type Octet
    = Octet Int


octet : Parser Octet
octet =
    let
        checkRange str =
            case String.toInt str of
                Just n ->
                    if 0 <= n && n < 256 then
                        Parser.succeed <| Octet n

                    else
                        Parser.problem "Each octet must be between 0 and 255"

                Nothing ->
                    Parser.problem "Each octet must be between 0 and 255"
    in
    Parser.getChompedString (Parser.chompWhile Char.isDigit) |> Parser.andThen checkRange


type IpAddress
    = IpAddress Octet Octet Octet Octet


ipAddress : Parser IpAddress
ipAddress =
    Parser.succeed IpAddress
        |= octet
        |. Parser.symbol "."
        |= octet
        |. Parser.symbol "."
        |= octet
        |. Parser.symbol "."
        |= octet


newIpAddress : String -> Maybe IpAddress
newIpAddress str =
    case Parser.run ipAddress str of
        Ok addr ->
            Just addr

        Err _ ->
            Nothing


ipAddressToInt : IpAddress -> Int
ipAddressToInt (IpAddress (Octet o1) (Octet o2) (Octet o3) (Octet o4)) =
    Bitwise.shiftLeftBy 24 o1 + Bitwise.shiftLeftBy 16 o2 + Bitwise.shiftLeftBy 8 o3 + o4


ipAddressToString : IpAddress -> String
ipAddressToString (IpAddress (Octet o1) (Octet o2) (Octet o3) (Octet o4)) =
    String.fromInt o1 ++ "." ++ String.fromInt o2 ++ "." ++ String.fromInt o3 ++ "." ++ String.fromInt o4


type PrefixLen
    = PrefixLen Int


newPrefixLen : String -> Maybe PrefixLen
newPrefixLen str =
    let
        checkRange len =
            if 8 <= len && len <= 30 then
                Just <| PrefixLen len

            else
                Nothing
    in
    String.toInt str |> Maybe.andThen checkRange


prefixLenToInt : PrefixLen -> Int
prefixLenToInt (PrefixLen len) =
    len


prefixLenToBitmask : PrefixLen -> Int
prefixLenToBitmask (PrefixLen len) =
    0xFFFFFFFF - (2 ^ (32 - len) - 1)


prefixLenToMask : PrefixLen -> String
prefixLenToMask prefixLen =
    let
        mask =
            prefixLenToBitmask prefixLen
    in
    (String.fromInt <| Bitwise.and 255 <| Bitwise.shiftRightBy 24 mask)
        ++ "."
        ++ (String.fromInt <| Bitwise.and 255 <| Bitwise.shiftRightBy 16 mask)
        ++ "."
        ++ (String.fromInt <| Bitwise.and 255 <| Bitwise.shiftRightBy 8 mask)
        ++ "."
        ++ (String.fromInt <| Bitwise.and 255 mask)


isHostAddr : IpAddress -> PrefixLen -> Bool
isHostAddr addr prefixLen =
    let
        hostMask =
            Bitwise.xor 0xFFFFFFFF <| prefixLenToBitmask prefixLen

        addrBits =
            ipAddressToInt addr

        hostBits =
            Bitwise.and addrBits hostMask
    in
    hostBits /= 0 && hostBits /= hostMask


type alias FactoryDevice =
    { address : IpAddress
    , gateway : IpAddress
    }


type alias FactoryDeviceInput =
    { address : String
    , gateway : String
    }


newFactoryDevice : FactoryDeviceInput -> Maybe FactoryDevice
newFactoryDevice input =
    Maybe.map2 FactoryDevice (newIpAddress input.address) (newIpAddress input.gateway)


newFactoryDevies : Array FactoryDeviceInput -> Maybe (Array FactoryDevice)
newFactoryDevies =
    Maybe.map Array.fromList << Maybe.Extra.combine << List.map newFactoryDevice << Array.toList


type alias Model =
    { gig1IpAddressInput : String
    , gig1IpAddressError : Maybe String
    , gig1IpAddress : Maybe IpAddress
    , gig1PrefixLen : PrefixLen
    , numberOfDevice : Int
    , factoryDeviceInputs : Array FactoryDeviceInput
    , factoryDevices : Maybe (Array FactoryDevice)
    , jsonOutput : String
    , debugString : String
    }


toJsonOutput : Model -> String
toJsonOutput model =
    let
        factoryDeviceToJsonValue : FactoryDevice -> Encode.Value
        factoryDeviceToJsonValue device =
            Encode.object
                [ ( "address", Encode.string <| ipAddressToString device.address )
                , ( "gateway", Encode.string <| ipAddressToString device.gateway )
                ]

        buildEncoderInput : IpAddress -> PrefixLen -> Array FactoryDevice -> List ( String, Encode.Value )
        buildEncoderInput addr prefix devices =
            [ ( "gig1IpAddress", Encode.string <| ipAddressToString addr )
            , ( "gig1PrefixLen", Encode.int <| prefixLenToInt prefix )
            , ( "gig1Netmask", Encode.string <| prefixLenToMask prefix )
            , ( "factoryDevices", Encode.array factoryDeviceToJsonValue devices )
            ]

        validate : Model -> Maybe (List ( String, Encode.Value ))
        validate m =
            Maybe.map3 buildEncoderInput m.gig1IpAddress (Just m.gig1PrefixLen) m.factoryDevices
    in
    case validate model of
        Just values ->
            Encode.encode 4 <| Encode.object values

        Nothing ->
            ""


generateJson : Model -> Model
generateJson model =
    { model | jsonOutput = toJsonOutput model }


init : Model
init =
    generateJson
        { gig1IpAddressInput = ""
        , gig1IpAddressError = Just errInvalidGig1IpAddress
        , gig1IpAddress = Nothing
        , gig1PrefixLen = PrefixLen 24
        , numberOfDevice = 1
        , factoryDeviceInputs = Array.fromList [ FactoryDeviceInput "" "" ]
        , factoryDevices = newFactoryDevies <| Array.fromList [ FactoryDeviceInput "" "" ]
        , jsonOutput = ""
        , debugString = ""
        }


errInvalidGig1IpAddress : String
errInvalidGig1IpAddress =
    "GE1 IP Address must be a valid IPv4 address (e.g. 192.168.100.1)."


errGig1IpAddressMustNotNetworkAddress : String
errGig1IpAddressMustNotNetworkAddress =
    "Neither network address nor broadcast address allowed.  Check Prefix Length too."



-- Update


type Msg
    = OnChangeGig1IpAddressInput String
    | OnChangeGig1PrefixLen PrefixLen
    | IncNumberOfDevice
    | DecNumberOfDevice
    | OnChangeFactoryDeviceInput Int String String


update : Msg -> Model -> Model
update msg model =
    let
        validateGi1IpAddress : String -> PrefixLen -> ( Maybe String, Maybe IpAddress )
        validateGi1IpAddress str prefixLen =
            case newIpAddress str of
                Nothing ->
                    ( Just errInvalidGig1IpAddress, Nothing )

                Just addr ->
                    if isHostAddr addr prefixLen then
                        ( Nothing, Just addr )

                    else
                        ( Just errGig1IpAddressMustNotNetworkAddress, Nothing )
    in
    case msg of
        OnChangeGig1IpAddressInput str ->
            let
                ( err, address ) =
                    validateGi1IpAddress str model.gig1PrefixLen
            in
            generateJson
                { model
                    | gig1IpAddressInput = str
                    , gig1IpAddressError = err
                    , gig1IpAddress = address
                }

        OnChangeGig1PrefixLen n ->
            let
                ( err, address ) =
                    validateGi1IpAddress model.gig1IpAddressInput n
            in
            generateJson
                { model
                    | gig1PrefixLen = n
                    , gig1IpAddressError = err
                    , gig1IpAddress = address
                }

        IncNumberOfDevice ->
            let
                newFactoryDeviceInputs =
                    Array.push (FactoryDeviceInput "" "") model.factoryDeviceInputs
            in
            if model.numberOfDevice >= 23 then
                model

            else
                generateJson
                    { model
                        | numberOfDevice = model.numberOfDevice + 1
                        , factoryDeviceInputs = newFactoryDeviceInputs
                        , factoryDevices = newFactoryDevies newFactoryDeviceInputs
                    }

        DecNumberOfDevice ->
            let
                newFactoryDeviceInputs =
                    Array.slice 0 (Array.length model.factoryDeviceInputs - 1) model.factoryDeviceInputs
            in
            if model.numberOfDevice <= 1 then
                model

            else
                generateJson
                    { model
                        | numberOfDevice = model.numberOfDevice - 1
                        , factoryDeviceInputs = newFactoryDeviceInputs
                        , factoryDevices = newFactoryDevies newFactoryDeviceInputs
                    }

        OnChangeFactoryDeviceInput index addr gw ->
            let
                updateFactoryDeviceInputs =
                    Array.set index { address = addr, gateway = gw } model.factoryDeviceInputs
            in
            generateJson
                { model
                    | factoryDeviceInputs = updateFactoryDeviceInputs
                    , factoryDevices = newFactoryDevies updateFactoryDeviceInputs
                }



-- View


view : Model -> Html Msg
view model =
    Element.layout [ padding 7 ] <|
        column []
            [ row [ padding 10 ] [ text "Kinetic GMM JSON Configuration Generator for IR809" ]
            , row [ padding 10, spacing 10 ] <| inputGe1Address model.gig1IpAddressInput model.gig1IpAddressError
            , row [ padding 10, spacing 10 ]
                [ inputGe1PrefixLen model.gig1PrefixLen, text <| String.fromInt <| prefixLenToInt model.gig1PrefixLen ]
            , row [ padding 10, spacing 10 ]
                [ el [ Font.bold ] <| text "Number of Device 1 - 23"
                , decNumberOfDevice
                , el [ width <| px 30 ] <| el [ Element.alignRight ] <| text <| String.fromInt model.numberOfDevice
                , incNumberOfDevice
                ]
            , row [ padding 10 ] [ inputFactoryDevices model.factoryDeviceInputs ]
            , row [ padding 10 ] [ text "Debug: ", text model.debugString ]
            , row [ width fill, Font.family [ Font.typeface "courier", Font.monospace ] ]
                [ el [ padding 7, Border.width 1, width fill ] <| text model.jsonOutput ]
            ]


inputGe1Address : String -> Maybe String -> List (Element Msg)
inputGe1Address str err =
    [ Input.text
        (case err of
            Just _ ->
                [ Border.color <| rgb255 255 0 0 ]

            Nothing ->
                []
        )
        { onChange = OnChangeGig1IpAddressInput
        , text = str
        , placeholder = Just <| Input.placeholder [] <| text "192.168.100.1"
        , label = Input.labelLeft [ centerY ] <| el [ Font.bold ] <| text "GE1 IP Addresss"
        }
    , el [ Font.color <| rgb255 255 0 0 ] <| text <| Maybe.withDefault "" err
    ]


inputGe1PrefixLen : PrefixLen -> Element Msg
inputGe1PrefixLen (PrefixLen n) =
    Input.slider
        [ width (px 300)
        , Element.behindContent
            (el
                [ width fill
                , Element.height (px 8)
                , centerY
                , Background.color <| rgb255 127 127 127
                , Border.rounded 2
                ]
                Element.none
            )
        ]
        { onChange = OnChangeGig1PrefixLen << PrefixLen << round
        , label = Input.labelLeft [ centerY ] <| el [ Font.bold ] <| text "Prefix Length 8 - 30"
        , min = 8
        , max = 30
        , value = toFloat n
        , thumb = Input.defaultThumb
        , step = Just 1
        }


incNumberOfDevice : Element Msg
incNumberOfDevice =
    Input.button [ padding 10, Border.width 1, Border.rounded 3 ]
        { onPress = Just IncNumberOfDevice
        , label = text " + "
        }


decNumberOfDevice : Element Msg
decNumberOfDevice =
    Input.button [ padding 10, Border.width 1, Border.rounded 3 ]
        { onPress = Just DecNumberOfDevice
        , label = text " - "
        }


inputFactoryDevices : Array FactoryDeviceInput -> Element Msg
inputFactoryDevices data =
    Element.indexedTable []
        { data = Array.toList data
        , columns =
            [ { header = text "Address"
              , width = fill
              , view = \n _ -> el [ centerY ] <| text <| "GW +" ++ String.fromInt (n + 7)
              }
            , { header = text "Device IP Address"
              , width = fill
              , view =
                    \n deviceInput ->
                        Input.text []
                            { onChange = \addr -> OnChangeFactoryDeviceInput n addr deviceInput.gateway
                            , text = deviceInput.address
                            , placeholder = Just <| Input.placeholder [] <| text <| "192.168.100." ++ String.fromInt (n + 2)
                            , label = Input.labelHidden "Device IP Address"
                            }
              }
            , { header = text "Gateway IP to the Device"
              , width = fill
              , view =
                    \n deviceInput ->
                        Input.text []
                            { onChange = OnChangeFactoryDeviceInput n deviceInput.address
                            , text = deviceInput.gateway
                            , placeholder = Just <| Input.placeholder [] <| text <| "192.168.100." ++ String.fromInt (n + 2)
                            , label = Input.labelHidden "Gateway IP to the Device"
                            }
              }
            ]
        }
