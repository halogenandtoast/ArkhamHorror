module Arkham.Skill.Cards.Beloved (beloved, Beloved (..)) where

import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (getSkillTestId)
import Arkham.Prelude
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype Beloved = Beloved SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beloved :: SkillCard Beloved
beloved = skillWith Beloved Cards.beloved (setMeta @Bool True)

instance RunMessage Beloved where
  runMessage msg s@(Beloved attrs) = case msg of
    RevealChaosToken _ _ token | token.face == #bless -> do
      getSkillTestId >>= \case
        Nothing -> pure s
        Just sid -> do
          let meta = toResult @Bool attrs.meta
          if meta
            then do
              player <- getPlayer attrs.owner
              push
                $ chooseOne
                  player
                  [ Label
                      "Remove Beloved from the game to replace that token's effects with the following: \"You automatically succeed. (Do not reveal another token. Return this token to the chaos bag after this test ends).\""
                      [ RemoveFromGame (toTarget attrs)
                      , skillTestModifier sid attrs (ChaosTokenTarget token) ReturnBlessedToChaosBag
                      , PassSkillTest
                      ]
                  , Label "Do not remove" []
                  ]
              pure $ Beloved $ attrs & setMeta @Bool False
            else pure s
    _ -> Beloved <$> runMessage msg attrs
