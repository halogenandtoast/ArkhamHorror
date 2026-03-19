module Arkham.Investigator.Cards.AndrePatel (andrePatel) where

import Arkham.Ability
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher hiding (DuringTurn)

newtype AndrePatel = AndrePatel InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

andrePatel :: InvestigatorCard AndrePatel
andrePatel =
  investigator AndrePatel Cards.andrePatel
    $ Stats {health = 7, sanity = 8, willpower = 2, intellect = 3, combat = 2, agility = 5}

instance HasAbilities AndrePatel where
  getAbilities (AndrePatel a) =
    [ playerLimit PerRound
        $ restricted a 1 (Self <> DuringTurn You)
        $ freeReaction (SkillTestResult #after You AnySkillTest (SuccessResult $ atLeast 2))
    ]

instance HasChaosTokenValue AndrePatel where
  getChaosTokenValue iid ElderSign (AndrePatel attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 2)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage AndrePatel where
  runMessage msg i@(AndrePatel attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      gainActions iid attrs 1
      pure i
    ElderSignEffect (is attrs -> True) -> do
      gainResources attrs.id ElderSign 1
      pure i
    _ -> AndrePatel <$> liftRunMessage msg attrs
