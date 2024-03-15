{- HLINT ignore "Use camelCase" -}
module Arkham.Story.Cards.UnfinishedBusiness_B (UnfinishedBusiness_B (..), unfinishedBusiness_B) where

import Arkham.Ability
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.SkillTest
import Arkham.Matcher
import Arkham.Placement
import Arkham.Prelude
import Arkham.SkillType
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype UnfinishedBusiness_B = UnfinishedBusiness_B StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unfinishedBusiness_B :: StoryCard UnfinishedBusiness_B
unfinishedBusiness_B = story UnfinishedBusiness_B Cards.unfinishedBusiness_B

instance HasAbilities UnfinishedBusiness_B where
  getAbilities (UnfinishedBusiness_B x) = case storyPlacement x of
    InThreatArea _ ->
      [ restrictedAbility x 1 (InThreatAreaOf You) $ forced $ RoundEnds #when
      , restrictedAbility
          x
          2
          (OnSameLocation <> exists (YourLocation <> "Chapel Crypt" <> LocationWithoutClues))
          actionAbility
      ]
    _ -> []

instance RunMessage UnfinishedBusiness_B where
  runMessage msg s@(UnfinishedBusiness_B attrs) = case msg of
    ResolveStory iid ResolveIt story' | story' == toId attrs -> do
      pure . UnfinishedBusiness_B $ attrs & placementL .~ InThreatArea iid
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ Label "Take 1 horror" [InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 1]
          , Label "Flip this back over" [Flip iid (toAbilitySource attrs 1) (toTarget attrs)]
          ]
      pure s
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ SkillLabel sType [beginSkillTest iid (attrs.ability 2) attrs sType 4]
          | sType <- [SkillIntellect, SkillCombat]
          ]
      pure s
    PassedThisSkillTest _ (isAbilitySource attrs 2 -> True) -> do
      card <- genCard Enemies.heretic_A
      send $ format card <> " is \"banished\""
      push $ AddToVictory (toTarget attrs)
      pure s
    Flip _ _ (isTarget attrs -> True) -> do
      let heretic = lookupCard Enemies.heretic_A (toCardId attrs)
      creation <- createEnemy heretic (storyPlacement attrs)
      pushAll [RemoveStory (toId attrs), toMessage creation]
      pure s
    _ -> UnfinishedBusiness_B <$> runMessage msg attrs
