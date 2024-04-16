import Test.Hspec ( hspec )

import FormeSpec as FS

main :: IO ()
main = hspec $ do
    -- FormeSpec
    FS.formeSpec
