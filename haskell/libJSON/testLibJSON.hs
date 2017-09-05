import           Control.Exception.Base (assert)
import           LibJSON
import           Test.QuickCheck

instance Arbitrary JValue where
  arbitrary = sized sizedJValue

t0 = JNum 10
t1 = JNum (-10)
t2 = JNull
t3 = JBool True
t4 = JBool False
t5 = JString "string"
t6 = JArray [JNum 10, JNum 20]
t7 = JArray [JArray [JNum 10, JNum 20]]
t8 = JArray [t7, t7]
t9 = JArray [JArray [JNum 10, JNum 20], t7, t8]
t10 = JObject [("field", JNum 10)]
t11 = JObject [("field", JNum 10), ("anotherField", JString "fieldValue")]
t12 = JObject [("field", JNum 10), ("anotherField", JString "fieldValue"), ("nestedObject", t11)]
t13 = JObject [("field", JNum 10), ("anotherField", JString "fieldValue"), ("nestedObject", t11), ("deeplyNestedObject", t12)]
t14 = JObject [("first", t13), ("second", t9), ("third", t12), ("fourth", t8)]
t15 = JArray [t0, t1, t2, t3, t4]
t16 = JArray [t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14]

testSuite :: [JValue]
testSuite = [t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16]

sizedJValue :: Int -> (Gen JValue)
sizedJValue 0 = elements testSuite
sizedJValue n = oneof [sizedJValue 0, sized sizedArbitraryJArray, sized sizedArbitraryJObject]

sizedArbitraryJArray :: Int -> (Gen JValue)
sizedArbitraryJArray 0 = do
  val <- sizedJValue 0
  return $ JArray [val]
sizedArbitraryJArray size = do
    head <- arbitrary
    JArray array <- sizedArbitraryJArray (size - 1)
    return $ JArray (head : array)

sizedArbitraryJObject :: Int -> (Gen JValue)
sizedArbitraryJObject 0 = do
  fieldName <- arbitrary
  val <- sizedJValue 0
  return $ JObject [(fieldName, val)]
sizedArbitraryJObject size = do
    fieldName <- arbitrary
    fieldValue <- arbitrary
    JObject array <- sizedArbitraryJObject (size - 1)
    return $ JObject ((fieldName, fieldValue) : array)

testParseJSON :: JValue -> Bool
testParseJSON jValue = (parseJSON (show jValue)) == jValue

main = do
  quickCheck testParseJSON
