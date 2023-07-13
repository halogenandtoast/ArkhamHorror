module Arkham.Investigator.Cards.SkidsOToole (
  SkidsOToole (..),
  skidsOToole,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message

newtype SkidsOToole = SkidsOToole InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

skidsOToole :: InvestigatorCard SkidsOToole
skidsOToole =
  investigator
    SkidsOToole
    Cards.skidsOToole
    Stats
      { health = 8
      , sanity = 6
      , willpower = 2
      , intellect = 3
      , combat = 3
      , agility = 4
      }

instance HasAbilities SkidsOToole where
  getAbilities (SkidsOToole a) =
    [ limitedAbility (PlayerLimit PerTurn 1) $
        restrictedAbility a 1 (Self <> DuringTurn You) $
          FastAbility $
            ResourceCost 2
    ]

instance HasChaosTokenValue SkidsOToole where
  getChaosTokenValue iid ElderSign (SkidsOToole attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 2)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage SkidsOToole where
  runMessage msg i@(SkidsOToole attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ GainActions (toId attrs) (toAbilitySource attrs 1) 1
      pure i
    PassedSkillTest iid _ _ (ChaosTokenTarget (chaosTokenFace -> ElderSign)) _ _ | iid == toId attrs -> do
      push $ TakeResources iid 2 (ChaosTokenEffectSource ElderSign) False
      pure i
    _ -> SkidsOToole <$> runMessage msg attrs
