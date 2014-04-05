module Infobeamer.ErrorMessages where

rawToXmlFail = "Could not convert raw data to XML."

xmlToRssFail = "Could not convert XML to RSS."

curlFail "CurlHttpReturnedError" = "HTTP(S) Response returned an error."
curlFail code = code
