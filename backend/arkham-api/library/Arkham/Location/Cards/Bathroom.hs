module Arkham.Location.Cards.Bathroom (bathroom) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (bathroom)
import Arkham.Location.Import.Lifted hiding (RevealChaosToken)
import Arkham.Matcher

newtype Bathroom = Bathroom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bathroom :: LocationCard Bathroom
bathroom = location Bathroom Cards.bathroom 1 (PerPlayer 1)

instance HasAbilities Bathroom where
  getAbilities (Bathroom a) =
    extendRevealed1 a
      $ restricted a 1 (DuringSkillTest $ WhileInvestigating $ be a)
      $ forced
      $ RevealChaosToken #after You
      $ mapOneOf ChaosTokenFaceIs [#skull, #cultist, #tablet, #autofail]

instance RunMessage Bathroom where
  runMessage msg l@(Bathroom attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      setActions iid (attrs.ability 1) 0
      endYourTurn iid
      pure l
    _ -> Bathroom <$> liftRunMessage msg attrs
