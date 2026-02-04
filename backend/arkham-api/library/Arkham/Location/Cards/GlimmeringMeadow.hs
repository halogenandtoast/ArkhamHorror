module Arkham.Location.Cards.GlimmeringMeadow (glimmeringMeadow) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.TheTwistedHollow.Helpers

newtype GlimmeringMeadow = GlimmeringMeadow LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

glimmeringMeadow :: LocationCard GlimmeringMeadow
glimmeringMeadow = locationWith GlimmeringMeadow Cards.glimmeringMeadow 2 (Static 0) connectsToAdjacent

instance HasAbilities GlimmeringMeadow where
  getAbilities (GlimmeringMeadow a) =
    extendRevealed
      a
      [ scenarioI18n $ withI18nTooltip "glimmeringMeadow.resign" $ locationResignAction a
      , playerLimit PerRound
          $ restricted a 1 (youExist $ HealableInvestigator (a.ability 1) #horror Anyone) actionAbility
      ]

instance RunMessage GlimmeringMeadow where
  runMessage msg l@(GlimmeringMeadow attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      healHorror iid (attrs.ability 1) 1
      pure l
    _ -> GlimmeringMeadow <$> liftRunMessage msg attrs
