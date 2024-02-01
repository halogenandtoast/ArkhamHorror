module Arkham.Investigator.Cards.SkidsOToole (
  SkidsOToole (..),
  skidsOToole,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher hiding (DuringTurn)

newtype SkidsOToole = SkidsOToole InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

skidsOToole :: InvestigatorCard SkidsOToole
skidsOToole =
  investigator SkidsOToole Cards.skidsOToole
    $ Stats {health = 8, sanity = 6, willpower = 2, intellect = 3, combat = 3, agility = 4}

instance HasAbilities SkidsOToole where
  getAbilities (SkidsOToole a) =
    [ playerLimit PerTurn
        $ restrictedAbility a 1 (Self <> DuringTurn You)
        $ FastAbility (ResourceCost 2)
    ]

instance HasChaosTokenValue SkidsOToole where
  getChaosTokenValue iid ElderSign (SkidsOToole attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 2)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage SkidsOToole where
  runMessage msg i@(SkidsOToole attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ GainActions iid (toAbilitySource attrs 1) 1
      pure i
    PassedSkillTestWithToken iid ElderSign | attrs `is` iid -> do
      push $ takeResources iid ElderSign 2
      pure i
    _ -> SkidsOToole <$> runMessage msg attrs
