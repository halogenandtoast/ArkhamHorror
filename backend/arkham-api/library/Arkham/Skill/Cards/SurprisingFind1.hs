module Arkham.Skill.Cards.SurprisingFind1 (surprisingFind1, surprisingFind1Effect, SurprisingFind1 (..)) where

import Arkham.Ability
import Arkham.Card
import Arkham.Effect.Import
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Placement
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Arkham.Trait (Trait (Research))

newtype SurprisingFind1 = SurprisingFind1 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

surprisingFind1 :: SkillCard SurprisingFind1
surprisingFind1 = skill SurprisingFind1 Cards.surprisingFind1

instance HasAbilities SurprisingFind1 where
  getAbilities (SurprisingFind1 x) =
    [ playerLimit (PerSearch Research) $ mkAbility x 1 $ freeReaction (AmongSearchedCards You)
    , controlledAbility
        x
        2
        (exists $ SkillWithId (toId x) <> NotSkill (SkillWithPlacement Limbo) <> EligibleSkill)
        Anytime
    ]

instance RunMessage SurprisingFind1 where
  runMessage msg s@(SurprisingFind1 attrs) = runQueueT $ case msg of
    InSearch (UseThisAbility iid (isSource attrs -> True) 1) -> do
      skillId <- getRandom
      pushAll
        [RemoveCardFromSearch iid (toCardId attrs), CreateSkill skillId (toCard attrs) iid (InPlayArea iid)]
      pure s
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withSkillTest \sid -> do
        skillTestModifier sid (toSource attrs) (toCardId attrs) MustBeCommitted
        push $ RemoveSkill (toId attrs)
        push $ SkillTestCommitCard iid (toCard attrs)
        createCardEffect Cards.surprisingFind1 Nothing (toSource attrs) iid
      pure s
    _ -> SurprisingFind1 <$> liftRunMessage msg attrs

newtype SurprisingFind1Effect = SurprisingFind1Effect EffectAttrs
  deriving anyclass (HasAbilities, HasModifiersFor, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

surprisingFind1Effect :: EffectArgs -> SurprisingFind1Effect
surprisingFind1Effect = cardEffect SurprisingFind1Effect Cards.surprisingFind1

instance RunMessage SurprisingFind1Effect where
  runMessage msg e@(SurprisingFind1Effect attrs) = runQueueT $ case msg of
    PassedSkillTest iid _ _ _ _ _ -> do
      drawCardsIfCan iid attrs.source 1
      disableReturn e
    SkillTestEnds {} -> disableReturn e
    _ -> SurprisingFind1Effect <$> liftRunMessage msg attrs
