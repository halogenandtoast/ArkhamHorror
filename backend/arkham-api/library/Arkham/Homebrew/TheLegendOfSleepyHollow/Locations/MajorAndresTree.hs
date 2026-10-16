module Arkham.Homebrew.TheLegendOfSleepyHollow.Locations.MajorAndresTree (majorAndresTree) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Homebrew.TheLegendOfSleepyHollow.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheLegendOfSleepyHollow.Helpers
import Arkham.Location.Import.Lifted

newtype MajorAndresTree = MajorAndresTree LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

majorAndresTree :: LocationCard MajorAndresTree
majorAndresTree = location MajorAndresTree Cards.majorAndresTree 2 (PerPlayer 1)

instance HasAbilities MajorAndresTree where
  getAbilities (MajorAndresTree a) =
    extendRevealed
      a
      [ scenarioI18n $ hauntedI "majorAndresTree.haunted" a 1
      , scenarioI18n $ withI18nTooltip "majorAndresTree.resign" $ locationResignAction a
      ]

instance RunMessage MajorAndresTree where
  runMessage msg l@(MajorAndresTree attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      nextTurnModifier iid (attrs.ability 1) iid (AnySkillValue (-1))
      pure l
    _ -> MajorAndresTree <$> liftRunMessage msg attrs
