import Data.Aeson.Lens

import JQ

-- foo = ("{\"a\":[true,false],\"b\":2,\"c\":3}" :: String) ^.. _JSON . jq mempty (If (Index "a" Do :| Values Do) (Index "b" Do) (Index "c" Do) :|= (Id :+ Number 42))
-- foo = ("{\"a\":[true,false],\"b\":[2,5],\"c\":3}" :: String) ^.. _JSON . jq mempty (If (Bool True `Comma` Bool False) (Index "a" Do) (Index "b" Do) :|= Values Do)
-- foo = ("{\"a\":[true,false],\"b\":[3,4],\"c\":3}" :: String) ^.. _JSON . jq mempty ((Number 42) :|= Values Do)
-- foo = ("[[true,0],[false,1],[true,2]]" :: String) ^.. _JSON . jq mempty (map' (select (Index (Number 0) Do)))
-- foo = ("[[true,0],[false,1],[true,2]]" :: String) & _JSON . jq mempty (Values Do :| (select (Index (Number 0) Do))) .~ "a"
-- foo = ("[1,2,[3,4],[],null,{\"a\":5,\"b\":6}]" :: String) ^.. _JSON . jq mempty ((Values Do :| Values Try) :|= Number 0)
-- foo = ("0" :: String) ^.. _JSON . jq mempty (Object [(String "a" `Comma` String "42", Number 1 `Comma` Number 2), (String "foo", Array Empty)])
-- foo = ("{\"a\":{\"b\":{\"c\":1}},\"b\":{\"c\":2}}" :: String) ^.. _JSON .  jq mempty ((Index (String "a") Do `Comma` (Index (String "b") Do :| Index (String "c") Do)) := ((Index (String "b") Do :| Index (String "c") Do) `Comma` Number 42))
-- foo = ("{\"a\":42}" :: String) ^.. _JSON . jq mempty ((Index (String "a") Do `Comma` Index (String "a") Do) :|= (Id :+ Number 1))
-- foo = ("[1,2,3]" :: String) ^.. _JSON . jq mempty (Index (String "a") Try :|= Null)
-- foo = ("null" :: String) ^.. _JSON . jq mempty (Number 42 :/ Number 0)
-- foo = ("[1,2,{\"a\":4,\"b\":\"hello\"}]" :: String) ^.. _JSON . jq mempty (Path (Index (Number 2) Do :| Index (Number 4) Do))
-- foo = ("[1,2,3]" :: String) ^.. _JSON . jq mempty (Bind "foo" (Number 1 `Comma` Number 2) (Index (Var "foo") Do))
-- foo = ("[0,2,3]" :: String) ^.. _JSON . jq mempty ((Index (Index (Number 0) Do) Do `Comma` Index (Index (Number 0) Do) Do) := Number 1)
foo = ("{\"a\":4,\"b\":null,\"c\":0}" :: String) ^.. _JSON . jq mempty (((Index (String "a") Do `Comma` Index (String "b") Do) :// Index (String "c") Do) := Number 42)

main = mapM_ print foo
