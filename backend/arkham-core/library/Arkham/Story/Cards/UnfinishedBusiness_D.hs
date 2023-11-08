module Arkham.Story.Cards.UnfinishedBusiness_D (
  UnfinishedBusiness_D (..),
  unfinishedBusiness_D,
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

newtype UnfinishedBusiness_D = UnfinishedBusiness_D StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unfinishedBusiness_D :: StoryCard UnfinishedBusiness_D
unfinishedBusiness_D = story UnfinishedBusiness_D Cards.unfinishedBusiness_D

instance HasAbilities UnfinishedBusiness_D where
  getAbilities (UnfinishedBusiness_D x) = case storyPlacement x of
    InThreatArea _ ->
      [ restrictedAbility x 1 (InThreatAreaOf You) $ ForcedAbility $ RoundEnds Timing.When
      , restrictedAbility
          x
          2
          ( OnSameLocation
              <> LocationExists (YourLocation <> LocationWithTitle "The Gallows" <> LocationWithoutClues)
          )
          $ ActionAbility []
          $ ActionCost 1
      ]
    _ -> []

instance RunMessage UnfinishedBusiness_D where
  runMessage msg s@(UnfinishedBusiness_D attrs) = case msg of
    ResolveStory iid ResolveIt story' | story' == toId attrs -> do
      pure . UnfinishedBusiness_D $ attrs & placementL .~ InThreatArea iid
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ Label "Take 1 damage" [InvestigatorAssignDamage iid (toSource attrs) DamageAny 1 0]
          , Label "Flip this back over" [Flip iid (toAbilitySource attrs 1) (toTarget attrs)]
          ]
      pure s
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ SkillLabel sType [beginSkillTest iid attrs attrs sType 4] | sType <- [SkillWillpower, SkillAgility]
          ]
      pure s
    PassedSkillTest _ _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ _ -> do
      card <- genCard Enemies.heretic_C
      send $ format card <> " is \"banished\""
      push $ AddToVictory (toTarget attrs)
      pure s
    Flip _ _ (isTarget attrs -> True) -> do
      heretic <- genCard Enemies.heretic_C
      creation <- createEnemy heretic (storyPlacement attrs)
      pushAll
        [RemoveStory (toId attrs), toMessage creation]
      pure s
    _ -> UnfinishedBusiness_D <$> runMessage msg attrs
