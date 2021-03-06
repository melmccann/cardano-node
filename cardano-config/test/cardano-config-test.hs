
import           Cardano.Prelude

import           Hedgehog.Main (defaultMain)

import qualified Test.Cardano.Config.Types
import qualified Test.Cardano.Config.Json

main :: IO ()
main =
  defaultMain
    [ Test.Cardano.Config.Json.tests
    , Test.Cardano.Config.Types.tests
    ]
