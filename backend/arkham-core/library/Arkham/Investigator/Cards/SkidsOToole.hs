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

instance HasTokenValue SkidsOToole where
  getTokenValue iid ElderSign (SkidsOToole attrs) | iid == toId attrs = do
    pure $ TokenValue ElderSign (PositiveModifier 2)
  getTokenValue _ token _ = pure $ TokenValue token mempty

instance RunMessage SkidsOToole where
  runMessage msg i@(SkidsOToole attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ GainActions (toId attrs) (toAbilitySource attrs 1) 1
      pure i
    PassedSkillTest iid _ _ (TokenTarget (tokenFace -> ElderSign)) _ _ | iid == toId attrs -> do
      push $ TakeResources iid 2 (TokenEffectSource ElderSign) False
      pure i
    _ -> SkidsOToole <$> runMessage msg attrs
