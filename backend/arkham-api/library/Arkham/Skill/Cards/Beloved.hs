module Arkham.Skill.Cards.Beloved (beloved) where

import Arkham.Helpers.SkillTest (getSkillTestId)
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype Beloved = Beloved SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beloved :: SkillCard Beloved
beloved = skillWith Beloved Cards.beloved (setMeta @Bool True)

instance RunMessage Beloved where
  runMessage msg s@(Beloved attrs) = runQueueT $ case msg of
    RevealChaosToken _ _ token | token.face == #bless -> do
      getSkillTestId >>= \case
        Nothing -> pure s
        Just sid ->
          if toResult @Bool attrs.meta
            then do
              chooseOneM attrs.owner do
                labeled
                  "Remove Beloved from the game to replace that token's effects with the following: \"You automatically succeed. (Do not reveal another token. Return this token to the chaos bag after this test ends).\""
                  do
                    removeFromGame attrs
                    skillTestModifier sid attrs (ChaosTokenTarget token) ReturnBlessedToChaosBag
                    passSkillTest
                labeled "Do not remove" nothing
              pure $ Beloved $ attrs & setMeta @Bool False
            else pure s
    _ -> Beloved <$> liftRunMessage msg attrs
