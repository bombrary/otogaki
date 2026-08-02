port module Ports exposing (fromAudio, saveToLocalStorage, toAudio)

import Json.Decode as Decode
import Json.Encode as Encode


port toAudio : Encode.Value -> Cmd msg


port fromAudio : (Decode.Value -> msg) -> Sub msg


port saveToLocalStorage : Encode.Value -> Cmd msg
