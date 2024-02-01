module Arkham.Story.Cards.UnfinishedBusiness_H (
  UnfinishedBusiness_H (..),
  unfinishedBusiness_H,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Discard
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Message.Discard
import Arkham.Helpers.SkillTest
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Placement
import Arkham.Projection
import Arkham.SkillType
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner
import Arkham.Timing qualified as Timing

newtype UnfinishedBusiness_H = UnfinishedBusiness_H StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

unfinishedBusiness_H :: StoryCard UnfinishedBusiness_H
unfinishedBusiness_H = story UnfinishedBusiness_H Cards.unfinishedBusiness_H

instance HasAbilities UnfinishedBusiness_H where
  getAbilities (UnfinishedBusiness_H x) = case storyPlacement x of
    InThreatArea _ ->
      [ restrictedAbility x 1 (InThreatAreaOf You) $ ForcedAbility $ RoundEnds Timing.When
      , restrictedAbility
          x
          2
          ( OnSameLocation
              <> LocationExists (YourLocation <> LocationWithTitle "Heretics' Graves" <> LocationWithoutClues)
          )
          $ ActionAbility []
          $ ActionCost 1
      ]
    _ -> []

instance RunMessage UnfinishedBusiness_H where
  runMessage msg s@(UnfinishedBusiness_H attrs) = case msg of
    ResolveStory iid ResolveIt story' | story' == toId attrs -> do
      pure . UnfinishedBusiness_H $ attrs & placementL .~ InThreatArea iid
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      hasCards <-
        fieldP
          InvestigatorHand
          ((>= 2) . length . filter (`cardMatch` NonWeakness))
          iid
      player <- getPlayer iid
      push
        $ chooseOne player
        $ [Label "Lose 2 resources" [toMessage $ discardFromHand iid attrs DiscardChoose 2] | hasCards]
        <> [ Label "Flip this back over" [Flip iid (toAbilitySource attrs 1) (toTarget attrs)]
           ]
      pure s
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ SkillLabel sType [beginSkillTest iid attrs attrs sType 4] | sType <- [SkillWillpower, SkillCombat]
          ]
      pure s
    PassedSkillTest _ _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ _ -> do
      card <- genCard Enemies.heretic_I
      send $ format card <> " is \"banished\""
      push $ AddToVictory (toTarget attrs)
      pure s
    Flip _ _ (isTarget attrs -> True) -> do
      heretic <- genCard Enemies.heretic_I
      creation <- createEnemy heretic (storyPlacement attrs)
      pushAll
        [RemoveStory (toId attrs), toMessage creation]
      pure s
    _ -> UnfinishedBusiness_H <$> runMessage msg attrs
