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
import Result.Extra



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


prefixLenToNetmask : PrefixLen -> String
prefixLenToNetmask prefixLen =
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


prefixLenToHostBitmask : PrefixLen -> Int
prefixLenToHostBitmask prefixLen =
    Bitwise.xor 0xFFFFFFFF <| prefixLenToBitmask prefixLen


isNetworkAddr : IpAddress -> PrefixLen -> Bool
isNetworkAddr addr prefixLen =
    Bitwise.and (ipAddressToInt addr) (prefixLenToHostBitmask prefixLen) == 0


isBroadcastAddr : IpAddress -> PrefixLen -> Bool
isBroadcastAddr addr prefixLen =
    let
        hostMask =
            prefixLenToHostBitmask prefixLen
    in
    Bitwise.and (ipAddressToInt addr) hostMask == hostMask


isSameNetwork : IpAddress -> IpAddress -> PrefixLen -> Bool
isSameNetwork addr1 addr2 prefixLen =
    let
        networkMask =
            prefixLenToBitmask prefixLen
    in
    Bitwise.and (ipAddressToInt addr1) networkMask == Bitwise.and (ipAddressToInt addr2) networkMask


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


type alias Gig1IpAddress =
    { addressInput : String
    , address : Result String IpAddress
    }


type alias FactoryDevice =
    { addressInput : String
    , address : Result String IpAddress
    , gatewayInput : String
    , gateway : Result String IpAddress
    }


emptyFactoryDevice : FactoryDevice
emptyFactoryDevice =
    { addressInput = ""
    , address = Err errSetGig1AddressFirst
    , gatewayInput = ""
    , gateway = Err errSetGig1AddressFirst
    }


type alias Model =
    { gig1IpAddress : Gig1IpAddress
    , gig1PrefixLen : PrefixLen
    , numberOfDevice : Int
    , factoryDevices : Array FactoryDevice

    -- , factoryDevices : Maybe (Array FactoryDevice)
    , jsonOutput : String
    , debugString : String
    }


toJsonOutput : Model -> String
toJsonOutput model =
    let
        combineDevices : Array FactoryDevice -> Result String (List ( IpAddress, IpAddress ))
        combineDevices devices =
            Result.Extra.combine <| List.map (\d -> Result.map2 Tuple.pair d.address d.gateway) <| Array.toList devices

        factoryDeviceToJsonValue : ( IpAddress, IpAddress ) -> Encode.Value
        factoryDeviceToJsonValue ( address, gateway ) =
            Encode.object
                [ ( "address", Encode.string <| ipAddressToString address )
                , ( "gateway", Encode.string <| ipAddressToString gateway )
                ]

        buildEncoderInput : IpAddress -> PrefixLen -> List ( IpAddress, IpAddress ) -> List ( String, Encode.Value )
        buildEncoderInput addr prefix devices =
            [ ( "gig1IpAddress", Encode.string <| ipAddressToString addr )
            , ( "gig1PrefixLen", Encode.int <| prefixLenToInt prefix )
            , ( "gig1Netmask", Encode.string <| prefixLenToNetmask prefix )
            , ( "factoryDevices", Encode.list factoryDeviceToJsonValue devices )
            ]

        validate : Model -> Result String (List ( String, Encode.Value ))
        validate m =
            Result.map3 buildEncoderInput m.gig1IpAddress.address (Ok m.gig1PrefixLen) (combineDevices m.factoryDevices)
    in
    case validate model of
        Err _ ->
            ""

        Ok values ->
            Encode.encode 4 <| Encode.object values


generateJson : Model -> Model
generateJson model =
    { model | jsonOutput = toJsonOutput model }


init : Model
init =
    generateJson
        { gig1IpAddress = Gig1IpAddress "" <| Err errInvalidIpAddress
        , gig1PrefixLen = PrefixLen 24
        , numberOfDevice = 1
        , factoryDevices = Array.fromList [ emptyFactoryDevice ]
        , jsonOutput = ""
        , debugString = ""
        }


errInvalidIpAddress : String
errInvalidIpAddress =
    "Input string must be a valid IPv4 address (e.g. 192.168.100.1)."


errNetworkAddressIsNotAllowed : String
errNetworkAddressIsNotAllowed =
    "Network address is not allowed.  Check Prefix Length too."


errBroadcastAddressIsNotAllowed : String
errBroadcastAddressIsNotAllowed =
    "Broadcast address is not allowed.  Check Prefix Length too."


errSetGig1AddressFirst : String
errSetGig1AddressFirst =
    "Set GE1 Address first."


errGig1AddressIsNotAllowed : String
errGig1AddressIsNotAllowed =
    "Cannot use GE1 address here."


errGatewayMustBelongGig1Network : String
errGatewayMustBelongGig1Network =
    "Gateway address must be a host within GE1 subnet."



-- Update


type Msg
    = OnChangeGig1IpAddressInput String
    | OnChangeGig1PrefixLen PrefixLen
    | IncNumberOfDevice
    | DecNumberOfDevice
    | OnChangeFactoryDeviceInput Int String String


update : Msg -> Model -> Model
update msg model =
    generateJson <|
        validateModel <|
            case msg of
                OnChangeGig1IpAddressInput str ->
                    let
                        input =
                            model.gig1IpAddress
                    in
                    { model | gig1IpAddress = { input | addressInput = str } }

                OnChangeGig1PrefixLen n ->
                    { model | gig1PrefixLen = n }

                IncNumberOfDevice ->
                    let
                        newFactoryDevices =
                            Array.push emptyFactoryDevice model.factoryDevices
                    in
                    if model.numberOfDevice >= 23 then
                        model

                    else
                        generateJson
                            { model
                                | numberOfDevice = model.numberOfDevice + 1
                                , factoryDevices = newFactoryDevices
                            }

                DecNumberOfDevice ->
                    let
                        newFactoryDevices =
                            Array.slice 0 (Array.length model.factoryDevices - 1) model.factoryDevices
                    in
                    if model.numberOfDevice <= 1 then
                        model

                    else
                        generateJson
                            { model
                                | numberOfDevice = model.numberOfDevice - 1
                                , factoryDevices = newFactoryDevices
                            }

                OnChangeFactoryDeviceInput index addrStr gwStr ->
                    let
                        updateFactoryDevices =
                            let
                                entry =
                                    { emptyFactoryDevice | addressInput = addrStr, gatewayInput = gwStr }
                            in
                            Array.set index entry model.factoryDevices
                    in
                    { model | factoryDevices = updateFactoryDevices }


validateModel : Model -> Model
validateModel model =
    let
        validateGig1Input : Model -> Model
        validateGig1Input m =
            let
                input =
                    m.gig1IpAddress

                validateGi1IpAddress : String -> PrefixLen -> Result String IpAddress
                validateGi1IpAddress str prefixLen =
                    case newIpAddress str of
                        Nothing ->
                            Err errInvalidIpAddress

                        Just decodedAddr ->
                            if isNetworkAddr decodedAddr prefixLen then
                                Err errNetworkAddressIsNotAllowed

                            else if isBroadcastAddr decodedAddr prefixLen then
                                Err errBroadcastAddressIsNotAllowed

                            else
                                Ok decodedAddr
            in
            { m | gig1IpAddress = { input | address = validateGi1IpAddress input.addressInput m.gig1PrefixLen } }

        rejectReservedAddress : IpAddress -> PrefixLen -> IpAddress -> Result String IpAddress
        rejectReservedAddress gig1addr prefixLen addr =
            if isNetworkAddr addr prefixLen then
                Err errNetworkAddressIsNotAllowed

            else if isBroadcastAddr addr prefixLen then
                Err errBroadcastAddressIsNotAllowed

            else if gig1addr == addr then
                Err errGig1AddressIsNotAllowed

            else
                Ok addr

        validateDeviceAddress : String -> ( Result String IpAddress, Bool )
        validateDeviceAddress str =
            case model.gig1IpAddress.address of
                Err _ ->
                    ( Err errSetGig1AddressFirst, False )

                Ok gig1addr ->
                    case newIpAddress str of
                        Nothing ->
                            ( Err errInvalidIpAddress, False )

                        Just addr ->
                            if not <| isSameNetwork addr gig1addr model.gig1PrefixLen then
                                ( Ok addr, False )

                            else
                                ( rejectReservedAddress gig1addr model.gig1PrefixLen addr, True )

        validateDeviceGateway : String -> Result String IpAddress
        validateDeviceGateway str =
            case model.gig1IpAddress.address of
                Err _ ->
                    Err errSetGig1AddressFirst

                Ok gig1addr ->
                    case newIpAddress str of
                        Nothing ->
                            Err errInvalidIpAddress

                        Just addr ->
                            if not <| isSameNetwork addr gig1addr model.gig1PrefixLen then
                                Err errGatewayMustBelongGig1Network

                            else
                                rejectReservedAddress gig1addr model.gig1PrefixLen addr

        validateDevice : FactoryDevice -> FactoryDevice
        validateDevice input =
            let
                ( deviceAddress, sameNetwork ) =
                    validateDeviceAddress input.addressInput
            in
            case deviceAddress of
                Err _ ->
                    { input | address = deviceAddress }

                Ok _ ->
                    if sameNetwork then
                        { input | address = deviceAddress, gatewayInput = input.addressInput, gateway = deviceAddress }

                    else
                        case validateDeviceGateway input.gatewayInput of
                            Err err ->
                                { input | address = deviceAddress, gatewayInput = input.gatewayInput, gateway = Err err }

                            Ok gw ->
                                { input | address = deviceAddress, gatewayInput = input.gatewayInput, gateway = Ok gw }

        validateDevices : Model -> Model
        validateDevices m =
            { m | factoryDevices = Array.map validateDevice m.factoryDevices }
    in
    validateDevices <| validateGig1Input model



-- View


view : Model -> Html Msg
view model =
    Element.layout [ padding 7 ] <|
        column []
            [ row [ padding 10 ] [ text "Kinetic GMM JSON Configuration Generator for IR809" ]
            , row [ padding 10, spacing 10 ] <| inputGe1Address model.gig1IpAddress
            , row [ padding 10, spacing 10 ]
                [ inputGe1PrefixLen model.gig1PrefixLen, text <| String.fromInt <| prefixLenToInt model.gig1PrefixLen ]
            , row [ padding 10, spacing 10 ]
                [ el [ Font.bold ] <| text "Number of Device 1 - 23"
                , decNumberOfDevice
                , el [ width <| px 30 ] <| el [ Element.alignRight ] <| text <| String.fromInt model.numberOfDevice
                , incNumberOfDevice
                ]
            , row [ padding 10 ] [ inputFactoryDevices model.factoryDevices ]
            , row [ padding 10 ] [ text "Debug: ", text model.debugString ]
            , row [ width fill, Font.family [ Font.typeface "courier", Font.monospace ] ]
                [ el [ padding 7, Border.width 1, width fill ] <| text model.jsonOutput ]
            ]


inputGe1Address : Gig1IpAddress -> List (Element Msg)
inputGe1Address input =
    let
        ( errColor, errMsg ) =
            case input.address of
                Ok _ ->
                    ( [], "" )

                Err err ->
                    ( [ Border.color <| rgb255 255 0 0 ], err )
    in
    [ Input.text
        errColor
        { onChange = OnChangeGig1IpAddressInput
        , text = input.addressInput
        , placeholder = Just <| Input.placeholder [] <| text "192.168.100.1"
        , label = Input.labelLeft [ centerY ] <| el [ Font.bold ] <| text "GE1 IP Addresss"
        }
    , el [ Font.color <| rgb255 255 0 0 ] <| text errMsg
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


inputFactoryDevices : Array FactoryDevice -> Element Msg
inputFactoryDevices data =
    let
        addressErrorColor dev =
            case dev.address of
                Ok _ ->
                    []

                Err _ ->
                    [ Border.color <| rgb255 255 0 0 ]

        gatewayErrorColor dev =
            case dev.gateway of
                Ok _ ->
                    []

                Err _ ->
                    [ Border.color <| rgb255 255 0 0 ]

        errorMessage dev =
            case dev.address of
                Err err ->
                    err

                Ok _ ->
                    case dev.gateway of
                        Err err ->
                            err

                        Ok _ ->
                            ""
    in
    Element.indexedTable [ spacing 5 ]
        { data = Array.toList data
        , columns =
            [ { header = text "Address"
              , width = fill
              , view = \n _ -> el [ centerY ] <| text <| "GW +" ++ String.fromInt (n + 7)
              }
            , { header = text "Device IP Address"
              , width = fill
              , view =
                    \n device ->
                        Input.text (addressErrorColor device)
                            { onChange = \addr -> OnChangeFactoryDeviceInput n addr device.gatewayInput
                            , text = device.addressInput
                            , placeholder = Just <| Input.placeholder [] <| text <| "192.168.100." ++ String.fromInt (n + 2)
                            , label = Input.labelHidden "Device IP Address"
                            }
              }
            , { header = text "Gateway IP to the Device"
              , width = fill
              , view =
                    \n device ->
                        Input.text (gatewayErrorColor device)
                            { onChange = OnChangeFactoryDeviceInput n device.addressInput
                            , text = device.gatewayInput
                            , placeholder = Just <| Input.placeholder [] <| text <| "192.168.100." ++ String.fromInt (n + 2)
                            , label = Input.labelHidden "Gateway IP to the Device"
                            }
              }
            , { header = text ""
              , width = fill
              , view =
                    \_ device ->
                        el [ centerY, Font.color <| rgb255 255 0 0 ] <| text <| errorMessage device
              }
            ]
        }
