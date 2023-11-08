module Arkham.Story.Cards.UnfinishedBusiness_F (
  UnfinishedBusiness_F (..),
  unfinishedBusiness_F,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.SkillTest
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Placement
import Arkham.Projection
import Arkham.SkillType
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner
import Arkham.Timing qualified as Timing

newtype UnfinishedBusiness_F = UnfinishedBusiness_F StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unfinishedBusiness_F :: StoryCard UnfinishedBusiness_F
unfinishedBusiness_F = story UnfinishedBusiness_F Cards.unfinishedBusiness_F

instance HasAbilities UnfinishedBusiness_F where
  getAbilities (UnfinishedBusiness_F x) = case storyPlacement x of
    InThreatArea _ ->
      [ restrictedAbility x 1 (InThreatAreaOf You) $ ForcedAbility $ RoundEnds Timing.When
      , restrictedAbility
          x
          2
          ( OnSameLocation
              <> LocationExists (YourLocation <> LocationWithTitle "Chapel Attic" <> LocationWithoutClues)
          )
          $ ActionAbility []
          $ ActionCost 1
      ]
    _ -> []

instance RunMessage UnfinishedBusiness_F where
  runMessage msg s@(UnfinishedBusiness_F attrs) = case msg of
    ResolveStory iid ResolveIt story' | story' == toId attrs -> do
      pure . UnfinishedBusiness_F $ attrs & placementL .~ InThreatArea iid
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      hasEnoughResources <- fieldMap InvestigatorResources (>= 2) iid
      player <- getPlayer iid
      push
        $ chooseOne player
        $ [Label "Lose 2 resources" [LoseResources iid (toSource attrs) 2] | hasEnoughResources]
        <> [ Label "Flip this back over" [Flip iid (toAbilitySource attrs 1) (toTarget attrs)]
           ]
      pure s
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ SkillLabel sType [beginSkillTest iid attrs attrs sType 4] | sType <- [SkillIntellect, SkillAgility]
          ]
      pure s
    PassedSkillTest _ _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ _ -> do
      card <- genCard Enemies.heretic_E
      send $ format card <> " is \"banished\""
      push $ AddToVictory (toTarget attrs)
      pure s
    Flip _ _ (isTarget attrs -> True) -> do
      heretic <- genCard Enemies.heretic_E
      creation <- createEnemy heretic (storyPlacement attrs)
      pushAll
        [RemoveStory (toId attrs), toMessage creation]
      pure s
    _ -> UnfinishedBusiness_F <$> runMessage msg attrs
