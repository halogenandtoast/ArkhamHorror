module Arkham.Types.Location.Cards.BurnedRuins_205
  ( burnedRuins_205
  , BurnedRuins_205(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (burnedRuins_205)
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol

newtype BurnedRuins_205 = BurnedRuins_205 LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

burnedRuins_205 :: LocationId -> BurnedRuins_205
burnedRuins_205 = BurnedRuins_205 . baseAttrs
  Cards.burnedRuins_205
  2
  (Static 3)
  Triangle
  [Square, Diamond]

instance HasModifiersFor env BurnedRuins_205 where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env BurnedRuins_205 where
  getActions = withDrawCardUnderneathAction

instance LocationRunner env => RunMessage env BurnedRuins_205 where
  runMessage msg (BurnedRuins_205 attrs) = case msg of
    AfterFailedInvestigate _ target | isTarget attrs target -> do
      pure
        . BurnedRuins_205
        $ (if locationClues attrs > 0
            then attrs & cluesL -~ 1 & doomL +~ 1
            else attrs
          )
    _ -> BurnedRuins_205 <$> runMessage msg attrs
