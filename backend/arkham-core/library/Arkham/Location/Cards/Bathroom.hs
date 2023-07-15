module Arkham.Location.Cards.Bathroom where

import Arkham.Prelude

import Arkham.Ability
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (bathroom)
import Arkham.Location.Runner hiding (RevealChaosToken)
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype Bathroom = Bathroom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bathroom :: LocationCard Bathroom
bathroom = location Bathroom Cards.bathroom 1 (PerPlayer 1)

instance HasAbilities Bathroom where
  getAbilities (Bathroom attrs) =
    withRevealedAbilities attrs $
      [ restrictedAbility
          attrs
          1
          (DuringSkillTest $ WhileInvestigating $ LocationWithId $ toId attrs)
          $ ForcedAbility
          $ RevealChaosToken Timing.After You
          $ ChaosTokenMatchesAny
          $ map ChaosTokenFaceIs [Skull, Cultist, Tablet, AutoFail]
      ]

instance RunMessage Bathroom where
  runMessage msg l@(Bathroom attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      l <$ pushAll [SetActions iid source 0, ChooseEndTurn iid]
    _ -> Bathroom <$> runMessage msg attrs
