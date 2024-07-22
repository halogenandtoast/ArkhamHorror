module Arkham.Skill.Cards.ThreeAces1 (threeAces1, ThreeAces1 (..)) where

import Arkham.Capability
import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype ThreeAces1 = ThreeAces1 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

threeAces1 :: SkillCard ThreeAces1
threeAces1 = skill ThreeAces1 Cards.threeAces1

instance RunMessage ThreeAces1 where
  runMessage msg (ThreeAces1 attrs) = case msg of
    InvestigatorCommittedSkill iid sid | sid == toId attrs -> do
      withSkillTest \stId -> do
        n <- selectCount $ skillIs Cards.threeAces1 <> skillControlledBy iid
        mods <- getModifiers (SkillTestTarget stId)
        when (n >= 3 && MetaModifier "ThreeAces1" `notElem` mods) $ do
          let drawing = drawCards iid attrs 3
          canDraw <- can.draw.cards iid
          canGainResources <- can.gain.resources iid
          pushAll
            $ [skillTestModifier stId (toSource attrs) sid (MetaModifier "ThreeAces1"), PassSkillTest]
            <> [drawing | canDraw]
            <> [takeResources iid (toSource attrs) 3 | canGainResources]
      ThreeAces1 <$> runMessage msg attrs
    _ -> ThreeAces1 <$> runMessage msg attrs
