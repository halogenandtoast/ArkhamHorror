module Arkham.Location.Cards.GlimmeringWoods (glimmeringWoods) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.TheTwistedHollow.Helpers

newtype GlimmeringWoods = GlimmeringWoods LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

glimmeringWoods :: LocationCard GlimmeringWoods
glimmeringWoods = locationWith GlimmeringWoods Cards.glimmeringWoods 2 (Static 0) connectsToAdjacent

instance HasAbilities GlimmeringWoods where
  getAbilities (GlimmeringWoods a) =
    extendRevealed
      a
      [ scenarioI18n $ withI18nTooltip "glimmeringWoods.resign" $ locationResignAction a
      , playerLimit PerRound
          $ restricted a 1 (youExist $ HealableInvestigator (a.ability 1) #horror Anyone) actionAbility
      ]

instance RunMessage GlimmeringWoods where
  runMessage msg l@(GlimmeringWoods attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      healHorror iid (UseAbilitySource iid (toSource attrs) 1) 1
      pure l
    _ -> GlimmeringWoods <$> liftRunMessage msg attrs
