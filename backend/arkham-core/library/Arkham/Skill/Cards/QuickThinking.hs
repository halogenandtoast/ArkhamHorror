module Arkham.Skill.Cards.QuickThinking (
  quickThinking,
  QuickThinking (..),
) where

import Arkham.Prelude

import Arkham.Card.CardCode
import Arkham.Classes
import Arkham.Message
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype QuickThinking = QuickThinking SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

quickThinking :: SkillCard QuickThinking
quickThinking = skill QuickThinking Cards.quickThinking

instance RunMessage QuickThinking where
  runMessage msg s@(QuickThinking attrs) = case msg of
    PassedSkillTest iid _ _ SkillTestInitiatorTarget {} _ n | n >= 2 -> do
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ Label
              "Take additional action"
              [ CreateEffect
                  (toCardCode attrs)
                  Nothing
                  (toSource attrs)
                  (InvestigatorTarget iid)
              ]
          , Label "Pass on additional action" []
          ]
      pure s
    _ -> QuickThinking <$> runMessage msg attrs
