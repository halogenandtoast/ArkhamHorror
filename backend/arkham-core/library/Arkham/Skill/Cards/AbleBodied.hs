module Arkham.Skill.Cards.AbleBodied (
  ableBodied,
  AbleBodied (..),
)
where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner
import Arkham.SkillType
import Arkham.Trait (Trait (Item))

newtype AbleBodied = AbleBodied SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ableBodied :: SkillCard AbleBodied
ableBodied =
  skill AbleBodied Cards.ableBodied

instance HasModifiersFor AbleBodied where
  getModifiersFor (CardIdTarget cid) (AbleBodied attrs) | toCardId attrs == cid =
    do
      itemAssets <- selectCount $ AssetWithTrait Item <> assetControlledBy (skillOwner attrs)
      pure
        $ toModifiers
          attrs
          [ AddSkillIcons
            $ if itemAssets <= 1
              then
                [ SkillIcon SkillCombat
                , SkillIcon SkillCombat
                , SkillIcon SkillAgility
                , SkillIcon SkillAgility
                ]
              else [SkillIcon SkillCombat, SkillIcon SkillAgility]
          | itemAssets <= 2
          ]
  getModifiersFor _ _ = pure []

instance RunMessage AbleBodied where
  runMessage msg (AbleBodied attrs) = AbleBodied <$> runMessage msg attrs
