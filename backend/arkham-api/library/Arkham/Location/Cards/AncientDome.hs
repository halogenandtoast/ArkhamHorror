module Arkham.Location.Cards.AncientDome (ancientDome) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.ObsidianCanyons.Helpers
import Arkham.Treachery.Cards qualified as Treacheries

newtype AncientDome = AncientDome LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientDome :: LocationCard AncientDome
ancientDome = location AncientDome Cards.ancientDome 2 (Static 0)

instance HasModifiersFor AncientDome where
  getModifiersFor (AncientDome a) = modifySelf a [CannotBeMoved, CannotLeavePlay]

instance HasAbilities AncientDome where
  getAbilities (AncientDome a) =
    extendRevealed a
      [ mkAbility a 1 $ forced $ RevealLocation #after You (be a)
      , scenarioI18n $ withI18nTooltip "ancientDome.resign" $ locationResignAction a
      ]

-- TODO: back (unrevealed) side Forced "When you would enter this location, if
-- you do not control the Obsidian Claw: spend 1 clue or test agility(2), else
-- cancel the move" belongs to the back of this location, not this front def.

instance RunMessage AncientDome where
  runMessage msg l@(AncientDome attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      erodedFrieze <- getSetAsideCard Treacheries.erodedFrieze
      drawCard iid erodedFrieze
      pure l
    _ -> AncientDome <$> liftRunMessage msg attrs
