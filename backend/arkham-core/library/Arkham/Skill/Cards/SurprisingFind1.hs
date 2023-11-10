module Arkham.Skill.Cards.SurprisingFind1 (
  surprisingFind1,
  SurprisingFind1 (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Placement
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner
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
        $ SilentForcedAbility AnyWindow
    ]

instance RunMessage SurprisingFind1 where
  runMessage msg s@(SurprisingFind1 attrs) = case msg of
    InSearch (UseThisAbility iid (isSource attrs -> True) 1) -> do
      skillId <- getRandom
      pushAll
        [RemoveCardFromSearch iid (toCardId attrs), CreateSkill skillId (toCard attrs) iid (InPlayArea iid)]
      pure s
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      pushAll
        [ skillTestModifier (toSource attrs) (toCardId attrs) MustBeCommitted
        , RemoveSkill (toId attrs)
        , SkillTestCommitCard iid (toCard attrs)
        ]
      pure s
    _ -> SurprisingFind1 <$> runMessage msg attrs
