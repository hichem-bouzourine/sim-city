import Test.Hspec ( hspec )

import FormeSpec as FS
import ZoneSpec as ZS

main :: IO ()
main = hspec $ do
    FS.formeSpec
    ZS.spec
