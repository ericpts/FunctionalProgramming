module LibJSON
  (
    JValue(..),
    show,
    parseJSON
  ) where

import           Data.Char   (isDigit, isPunctuation, isSpace)
import           Data.List   (intercalate, isPrefixOf)
import           Debug.Trace (trace)

data JValue =
  JString String
  | JNum Double
  | JBool Bool
  | JNull
  | JArray [JValue]
  | JObject [(String, JValue)]
  deriving (Eq)

quote :: String -> String
quote string = "\"" ++ string ++ "\""

instance Show(JValue) where
  show (JString string) = quote string
  show (JNum double)    = show double
  show JNull            = "null"
  show (JBool True)     = "true"
  show (JBool False)    = "false"
  show (JArray array)   = "[" ++ intercalate ", " (map show array) ++ "]"
  show (JObject object) = "{" ++ intercalate ", " (map showField object) ++ "}"
    where showField (string, value) = quote string ++ ":" ++ show value


stripWhitespace :: String -> String
stripWhitespace string = snd $ span isSpace string

parseJNum :: String -> (JValue, String)
-- parseJNum string | trace ("parseJNum-" ++ string) False = undefined
parseJNum string = let (numberString, rest) = break (\x -> isSpace x || x == '}' || x == ']' || x == ',') string
                       in (JNum (read numberString :: Double), rest)

parseJString :: String -> (JValue, String)
-- parseJString string | trace ("parseJString-" ++ string) False = undefined
parseJString ('"':string) = let (ret, _:rest) = break ( == '"') string in (JString ret, rest)

parseJBool :: String -> (JValue, String)
-- parseJBool string | trace ("parseJBool-" ++ string) False = undefined
parseJBool string
  | "true" `isPrefixOf` string = (JBool True, drop (length "true") string)
  | "false" `isPrefixOf` string = (JBool False, drop (length "false") string)

parseJNull :: String -> (JValue, String)
-- parseJNull string | trace ("parseJNull-" ++ string) False = undefined
parseJNull string
  | "null" `isPrefixOf` string = (JNull, drop (length "null") string)

isNumberStart :: String -> Bool
isNumberStart ('-':_) = True
isNumberStart (c:_)   = isDigit c

parseJValue :: String -> (JValue, String)
-- parseJValue string | trace ("parseJValue-" ++ string) False = undefined
parseJValue string@(' ':_) = parseJValue $ stripWhitespace string
parseJValue string@('\n':_) = parseJValue $ stripWhitespace string
parseJValue string@('t':_) = parseJBool string
parseJValue string@('f':_) = parseJBool string
parseJValue string@('n':_) = parseJNull string
parseJValue string@('[':_) = parseJArray string
parseJValue string@('{':_) = parseJObject string
parseJValue string@('"':_) = parseJString string
parseJValue string
  | isNumberStart string = parseJNum string
  | otherwise = error "Trying to parse invalid JSON"

parseJArray :: String -> (JValue, String)
-- parseJArray string | trace ("parseJArray-" ++ string) False = undefined
parseJArray ('[':string) = let (array, rest) = evalArrayElements string in (JArray array, rest)
  where evalArrayElements (']':string) = ([], string)
        evalArrayElements string = let (val, rest) = parseJValue string in
                                     case rest of
                                       (',':xs) -> let (nextArray, stringRet) = evalArrayElements (stripWhitespace xs) in (val:nextArray, stringRet)
                                       (']':xs) -> ([val], xs)

parseJObject :: String -> (JValue, String)
-- parseJObject string | trace ("parseJObject-" ++ string) False = undefined
parseJObject ('{':string) = let (array, rest) = evalObjectElements string in (JObject array, rest)
  where evalObjectElements ('}':string) = ([], string)
        evalObjectElements string = let (JString fieldName, rest) = parseJString string in
                                      case rest of
                                        (':':afterColon) -> let (fieldValue, stringRest) = parseJValue afterColon in
                                                      case stringRest of
                                                        (',':xs) -> let (nextArray, stringRet) = evalObjectElements (stripWhitespace xs) in ((fieldName, fieldValue):nextArray, stringRet)
                                                        ('}':xs) -> ([(fieldName, fieldValue)], xs)
                                                        _ -> error "Trying to parse invalid JSON object!"
                                        _ -> error "Trying to parse invalid JSON object!"

parseJSON :: String -> JValue
parseJSON string = fst (parseJValue string)
