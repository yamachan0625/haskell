getRequestUrl host apiKey resource id = host ++ "/" ++ resource ++ "/" ++ id ++ "?token=" ++ apiKey

genHostRequestBuilder host = (\apiKey resource id -> getRequestUrl host apiKey resource id )

exampleBuilder = genHostRequestBuilder "http://example.com"

genApiRequestsBuilder hostBuilder apiKey = (\resource id -> hostBuilder apiKey resource id)

myExampleUrlBuilder = genApiRequestsBuilder exampleBuilder "{apiKey}"


add4 a b c d = a + b + c + d 
mystery = add4 5 -- 引数b c dを待機する新しいかんすmysteryが作られる

subtracttt = flip (-) 2



bbb bf arg = (\x -> bf arg x)
ccc = bbb (-) 4
-- ccc 10 と呼び出す

