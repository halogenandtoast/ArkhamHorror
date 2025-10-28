module Arkham.Location.Cards.LodgeGatesWeveBeenExpectingYou (lodgeGatesWeveBeenExpectingYou) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.ForTheGreaterGood.Helpers

newtype LodgeGatesWeveBeenExpectingYou = LodgeGatesWeveBeenExpectingYou LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lodgeGatesWeveBeenExpectingYou :: LocationCard LodgeGatesWeveBeenExpectingYou
lodgeGatesWeveBeenExpectingYou = location LodgeGatesWeveBeenExpectingYou Cards.lodgeGatesWeveBeenExpectingYou 2 (Static 0)

instance HasModifiersFor LodgeGatesWeveBeenExpectingYou where
  getModifiersFor (LodgeGatesWeveBeenExpectingYou a) =
    whenRevealed a $ modifySelect a AnyEnemy [CannotSpawnIn (be a)]

instance HasAbilities LodgeGatesWeveBeenExpectingYou where
  getAbilities (LodgeGatesWeveBeenExpectingYou a) =
    extendRevealed1 a
      $ scenarioI18n
      $ withI18nTooltip "lodgeGatesWeveBeenExpectingYou.resign" (locationResignAction a)

instance RunMessage LodgeGatesWeveBeenExpectingYou where
  runMessage msg (LodgeGatesWeveBeenExpectingYou attrs) =
    LodgeGatesWeveBeenExpectingYou <$> runMessage msg attrs
