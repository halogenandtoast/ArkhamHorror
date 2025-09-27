module Arkham.Location.Cards.HangmansBrookSpectral (hangmansBrookSpectral) where

import Arkham.Ability
import Arkham.Card
import Arkham.Constants
import Arkham.GameValue
import Arkham.Helpers.Location
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Scenarios.TheWagesOfSin.Helpers

newtype HangmansBrookSpectral = HangmansBrookSpectral LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hangmansBrookSpectral :: LocationCard HangmansBrookSpectral
hangmansBrookSpectral = location HangmansBrookSpectral Cards.hangmansBrookSpectral 1 (Static 0)

instance HasAbilities HangmansBrookSpectral where
  getAbilities (HangmansBrookSpectral a) =
    extendRevealed
      a
      [ scenarioI18n $ withI18nTooltip "hangmansBrookSpectral.resign" (locationResignAction a)
      , scenarioI18n $ hauntedI "hangmansBrookSpectral.haunted" a 1
      ]

instance RunMessage HangmansBrookSpectral where
  runMessage msg l@(HangmansBrookSpectral attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 1
      roundModifier attrs iid (CannotTriggerAbilityMatching $ AbilityIs (toSource attrs) ResignAbility)
      pure l
    FlipThis (isTarget attrs -> True) -> do
      swapLocation attrs =<< genCard Locations.hangmansBrook
      pure l
    _ -> HangmansBrookSpectral <$> liftRunMessage msg attrs
