module Arkham.Treachery.Cards.BloodOnYourHands (
  bloodOnYourHands,
  BloodOnYourHands (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.Trait (Trait (CrimeScene, Innocent))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype BloodOnYourHands = BloodOnYourHands TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bloodOnYourHands :: TreacheryCard BloodOnYourHands
bloodOnYourHands = treachery BloodOnYourHands Cards.bloodOnYourHands

instance RunMessage BloodOnYourHands where
  runMessage msg t@(BloodOnYourHands attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      innocents <- selectCount $ VictoryDisplayCardMatch $ CardWithTrait Innocent
      push $ beginSkillTest iid (toSource attrs) iid #willpower (2 + innocents)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      atCrimeScene <- iid <=~> InvestigatorAt (LocationWithTrait CrimeScene)
      pushAll $ assignHorror iid (toSource attrs) 2
        : [toMessage $ chooseAndDiscardCard iid (toSource attrs) | atCrimeScene]
      pure t
    _ -> BloodOnYourHands <$> runMessage msg attrs
