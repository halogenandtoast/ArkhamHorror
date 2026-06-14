module Arkham.Location.Cards.WesternWall_11651 (westernWall_11651) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.ObsidianCanyons.Helpers
import Arkham.Treachery.Cards qualified as Treacheries

newtype WesternWall_11651 = WesternWall_11651 LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

westernWall_11651 :: LocationCard WesternWall_11651
westernWall_11651 = location WesternWall_11651 Cards.westernWall_11651 2 (Static 0)

instance HasModifiersFor WesternWall_11651 where
  getModifiersFor (WesternWall_11651 a) = modifySelf a [CannotBeMoved, CannotLeavePlay]

instance HasAbilities WesternWall_11651 where
  getAbilities (WesternWall_11651 a) =
    extendRevealed a
      [ mkAbility a 1 $ forced $ RevealLocation #after You (be a)
      , scenarioI18n $ withI18nTooltip "westernWall.resign" $ locationResignAction a
      ]

instance RunMessage WesternWall_11651 where
  runMessage msg l@(WesternWall_11651 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      erodedFrieze <- getSetAsideCard Treacheries.erodedFrieze
      drawCard iid erodedFrieze
      pure l
    _ -> WesternWall_11651 <$> liftRunMessage msg attrs
