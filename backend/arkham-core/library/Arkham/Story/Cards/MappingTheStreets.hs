module Arkham.Story.Cards.MappingTheStreets (
  MappingTheStreets (..),
  mappingTheStreets,
) where

import Arkham.Prelude

import Arkham.DamageEffect
import Arkham.Game.Helpers
import Arkham.Helpers.SkillTest
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.SkillType
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype MappingTheStreets = MappingTheStreets StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mappingTheStreets :: StoryCard MappingTheStreets
mappingTheStreets = story MappingTheStreets Cards.mappingTheStreets

instance RunMessage MappingTheStreets where
  runMessage msg s@(MappingTheStreets attrs) = case msg of
    ResolveStory iid story' | story' == toId attrs -> do
      hastur <- selectJust $ EnemyWithTitle "Hastur"
      n <- perPlayer 1
      pushAll
        [ beginSkillTest
            iid
            (toSource attrs)
            (InvestigatorTarget iid)
            SkillIntellect
            3
        , Msg.EnemyDamage hastur $ storyDamage iid n
        ]
      pure s
    _ -> MappingTheStreets <$> runMessage msg attrs
