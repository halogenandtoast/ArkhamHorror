module Arkham.Location.Cards.GreenRoom (
  greenRoom,
  GreenRoom (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Investigate
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner

newtype GreenRoom = GreenRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

greenRoom :: LocationCard GreenRoom
greenRoom = location GreenRoom Cards.greenRoom 5 (PerPlayer 1)

instance HasAbilities GreenRoom where
  getAbilities (GreenRoom attrs) =
    withRevealedAbilities attrs
      $ [ withTooltip
            "{action} _Investigate_. You get +3 {intellect} for this investigation. After this skill test ends, discard each card in your hand."
            $ investigateAbility attrs 1 mempty Here
        ]

instance RunMessage GreenRoom where
  runMessage msg l@(GreenRoom attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = toAbilitySource attrs 1
      investigation <- mkInvestigate iid source
      pushAll
        [ skillTestModifier source iid (SkillModifier #intellect 3)
        , toMessage investigation
        , DiscardHand iid source
        ]
      pure l
    _ -> GreenRoom <$> runMessage msg attrs
