module Arkham.Location.Cards.HangmansBrookSpectral (
  hangmansBrookSpectral,
  HangmansBrookSpectral (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Constants
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Runner
import Arkham.Matcher

newtype HangmansBrookSpectral = HangmansBrookSpectral LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

hangmansBrookSpectral :: LocationCard HangmansBrookSpectral
hangmansBrookSpectral = location HangmansBrookSpectral Cards.hangmansBrookSpectral 1 (Static 0)

instance HasAbilities HangmansBrookSpectral where
  getAbilities (HangmansBrookSpectral a) =
    withRevealedAbilities
      a
      [ withTooltip
          "\"Who's bright idea was this, anyway?\""
          (locationResignAction a)
      , haunted
          "Take 1 damage. Until the end of the round, the above resign ability cannot be triggered."
          a
          1
      ]

instance RunMessage HangmansBrookSpectral where
  runMessage msg l@(HangmansBrookSpectral attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      pushAll
        [ InvestigatorAssignDamage iid (toSource attrs) DamageAny 1 0
        , roundModifier attrs iid (CannotTriggerAbilityMatching $ AbilityIs (toSource attrs) ResignAbility)
        ]
      pure l
    Flip _ _ target | isTarget attrs target -> do
      regular <- genCard Locations.hangmansBrook
      push $ ReplaceLocation (toId attrs) regular Swap
      pure l
    _ -> HangmansBrookSpectral <$> runMessage msg attrs
