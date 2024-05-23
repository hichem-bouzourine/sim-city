import Test.Hspec ( hspec )

import FormeSpec as FS
import ZoneSpec as ZS
import VilleSpec as VS
import BatimentSpec as BS
import CitoyenSpec as CS
import EnvironnementSpec as ES
import GraphSpec as GS

main :: IO ()
main = hspec $ do
    FS.formeSpec
    ZS.spec
    VS.villeSpec
    BS.batimentSpec
    CS.citoyenSpec
    ES.environnementSpec
    GS.graphSpec