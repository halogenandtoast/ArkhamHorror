module Arkham.Location.Cards.HangmansBrookSpectral (hangmansBrookSpectral, HangmansBrookSpectral (..)) where

import Arkham.Ability
import Arkham.Card
import Arkham.Constants
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message (ReplaceStrategy (..))
import Arkham.Modifier

newtype HangmansBrookSpectral = HangmansBrookSpectral LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hangmansBrookSpectral :: LocationCard HangmansBrookSpectral
hangmansBrookSpectral = location HangmansBrookSpectral Cards.hangmansBrookSpectral 1 (Static 0)

instance HasAbilities HangmansBrookSpectral where
  getAbilities (HangmansBrookSpectral a) =
    extendRevealed
      a
      [ withTooltip "\"Who's bright idea was this, anyway?\"" (locationResignAction a)
      , haunted
          "Take 1 damage. Until the end of the round, the above resign ability cannot be triggered."
          a
          1
      ]

instance RunMessage HangmansBrookSpectral where
  runMessage msg l@(HangmansBrookSpectral attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 1
      roundModifier attrs iid (CannotTriggerAbilityMatching $ AbilityIs (toSource attrs) ResignAbility)
      pure l
    Flip _ _ (isTarget attrs -> True) -> do
      regular <- genCard Locations.hangmansBrook
      push $ ReplaceLocation (toId attrs) regular Swap
      pure l
    _ -> HangmansBrookSpectral <$> liftRunMessage msg attrs
