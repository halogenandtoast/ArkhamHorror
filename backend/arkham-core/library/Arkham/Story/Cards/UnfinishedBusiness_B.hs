module Arkham.Story.Cards.UnfinishedBusiness_B (
  UnfinishedBusiness_B (..),
  unfinishedBusiness_B,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.SkillTest
import Arkham.Matcher
import Arkham.Placement
import Arkham.SkillType
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner
import Arkham.Timing qualified as Timing

newtype UnfinishedBusiness_B = UnfinishedBusiness_B StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unfinishedBusiness_B :: StoryCard UnfinishedBusiness_B
unfinishedBusiness_B = story UnfinishedBusiness_B Cards.unfinishedBusiness_B

instance HasAbilities UnfinishedBusiness_B where
  getAbilities (UnfinishedBusiness_B x) = case storyPlacement x of
    InThreatArea _ ->
      [ restrictedAbility x 1 (InThreatAreaOf You) $ ForcedAbility $ RoundEnds Timing.When
      , restrictedAbility
          x
          2
          ( OnSameLocation
              <> LocationExists (YourLocation <> LocationWithTitle "Chapel Crypt" <> LocationWithoutClues)
          )
          $ ActionAbility Nothing
          $ ActionCost 1
      ]
    _ -> []

instance RunMessage UnfinishedBusiness_B where
  runMessage msg s@(UnfinishedBusiness_B attrs) = case msg of
    ResolveStory iid ResolveIt story' | story' == toId attrs -> do
      pure . UnfinishedBusiness_B $ attrs & placementL .~ InThreatArea iid
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push
        $ chooseOne
          iid
          [ Label "Take 1 horror" [InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 1]
          , Label "Flip this back over" [Flip iid (toAbilitySource attrs 1) (toTarget attrs)]
          ]
      pure s
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      push
        $ chooseOne
          iid
          [SkillLabel sType [beginSkillTest iid attrs attrs sType 4] | sType <- [SkillIntellect, SkillCombat]]
      pure s
    PassedSkillTest _ _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ _ -> do
      card <- genCard Enemies.heretic_A
      send $ format card <> " is \"banished\""
      push $ AddToVictory (toTarget attrs)
      pure s
    Flip _ _ (isTarget attrs -> True) -> do
      let heretic = lookupCard Enemies.heretic_A (toCardId attrs)
      creation <- createEnemy heretic (storyPlacement attrs)
      pushAll
        [RemoveStory (toId attrs), toMessage creation]
      pure s
    _ -> UnfinishedBusiness_B <$> runMessage msg attrs
