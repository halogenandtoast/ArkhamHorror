module Arkham.Skill.Cards.SurprisingFind1 (surprisingFind1) where

import Arkham.Ability
import Arkham.Card
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
    , controlled
        x
        2
        ( DuringYourSkillTest
            <> exists (SkillWithId (toId x) <> NotSkill (SkillWithPlacement Limbo) <> EligibleSkill)
        )
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
        onSucceedByEffect sid AnyValue (attrs.ability 2) sid $ drawCards iid (attrs.ability 2) 1
      pure s
    _ -> SurprisingFind1 <$> liftRunMessage msg attrs
