module Arkham.Story.Cards.StepsOfThePalace (
  StepsOfThePalace (..),
  stepsOfThePalace,
) where

import Arkham.Prelude

import Arkham.DamageEffect
import Arkham.Game.Helpers
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype StepsOfThePalace = StepsOfThePalace StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stepsOfThePalace :: StoryCard StepsOfThePalace
stepsOfThePalace = story StepsOfThePalace Cards.stepsOfThePalace

instance RunMessage StepsOfThePalace where
  runMessage msg s@(StepsOfThePalace attrs) = case msg of
    ResolveStory iid story' | story' == toId attrs -> do
      hastur <- selectJust $ EnemyWithTitle "Hastur"
      investigatorIds <- selectList $ investigatorEngagedWith hastur
      n <- perPlayer 1
      pushAll $
        [ Msg.EnemyDamage hastur $ storyDamage iid n
        , Exhaust (EnemyTarget hastur)
        ]
          <> [DisengageEnemy iid' hastur | iid' <- investigatorIds]
      pure s
    _ -> StepsOfThePalace <$> runMessage msg attrs
