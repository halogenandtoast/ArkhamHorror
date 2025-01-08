module Arkham.Location.Cards.LodgeGatesWeveBeenExpectingYou (
  lodgeGatesWeveBeenExpectingYou,
  LodgeGatesWeveBeenExpectingYou (..),
)
where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype LodgeGatesWeveBeenExpectingYou = LodgeGatesWeveBeenExpectingYou LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lodgeGatesWeveBeenExpectingYou :: LocationCard LodgeGatesWeveBeenExpectingYou
lodgeGatesWeveBeenExpectingYou = location LodgeGatesWeveBeenExpectingYou Cards.lodgeGatesWeveBeenExpectingYou 2 (Static 0)

instance HasModifiersFor LodgeGatesWeveBeenExpectingYou where
  getModifiersFor (LodgeGatesWeveBeenExpectingYou a) =
    whenRevealed a $ modifySelect a AnyEnemy [CannotSpawnIn (be a)]

instance HasAbilities LodgeGatesWeveBeenExpectingYou where
  getAbilities (LodgeGatesWeveBeenExpectingYou attrs) =
    extendRevealed1 attrs
      $ withTooltip "On second thought, maybe coming here was a bad idea" (locationResignAction attrs)

instance RunMessage LodgeGatesWeveBeenExpectingYou where
  runMessage msg (LodgeGatesWeveBeenExpectingYou attrs) =
    LodgeGatesWeveBeenExpectingYou <$> runMessage msg attrs
