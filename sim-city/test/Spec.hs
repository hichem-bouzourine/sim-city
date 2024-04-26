import Test.Hspec ( hspec )

import FormeSpec as FS
import ZoneSpec as ZS
import VilleSpec as VS
import BatimentSpec as BS
import CitoyenSpec as CS

main :: IO ()
main = hspec $ do
    FS.formeSpec
    ZS.spec
    VS.villeSpec
    BS.batimentSpec
    CS.citoyenSpec