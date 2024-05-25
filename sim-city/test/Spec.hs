import Test.Hspec ( hspec )

import FormeSpec as FS
import ZoneSpec as ZS
import VilleSpec as VS
import BatimentSpec as BS
import CitoyenSpec as CS
import EnvironnementSpec as ES
import GraphSpec as GS
import FormQuickCheck as FQ
import BatimentQuickCheck as BQ
import CitoyenQuickCheck as CQ
import ZoneQuickCheck as ZQ

main :: IO ()
main = hspec $ do
    ZS.spec
    VS.villeSpec
    BS.batimentSpec
    CS.citoyenSpec
    ES.environnementSpec
    GS.graphSpec
    FQ.quickTests
    BQ.quickTests
    CQ.quickTests
    ZQ.quickTests