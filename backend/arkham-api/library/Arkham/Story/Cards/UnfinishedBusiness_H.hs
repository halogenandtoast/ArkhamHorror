{- HLINT ignore "Use camelCase" -}
module Arkham.Story.Cards.UnfinishedBusiness_H (unfinishedBusiness_H) where

import Arkham.Ability
import Arkham.Card
import Arkham.Discard
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Message.Discard.Lifted (discardFromHand)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Projection
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted
import Arkham.Target

newtype UnfinishedBusiness_H = UnfinishedBusiness_H StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unfinishedBusiness_H :: StoryCard UnfinishedBusiness_H
unfinishedBusiness_H = story UnfinishedBusiness_H Cards.unfinishedBusiness_H

instance HasAbilities UnfinishedBusiness_H where
  getAbilities (UnfinishedBusiness_H x) = case x.placement of
    InThreatArea _ ->
      [ restricted x 1 (InThreatAreaOf You) $ forced $ RoundEnds #when
      , skillTestAbility
          $ restricted
            x
            2
            ( OnSameLocation
                <> exists (YourLocation <> "Heretics' Graves" <> LocationWithoutClues)
            )
            actionAbility
      ]
    _ -> []

instance RunMessage UnfinishedBusiness_H where
  runMessage msg s@(UnfinishedBusiness_H attrs) = runQueueT $ case msg of
    ResolveStory iid ResolveIt story' | story' == toId attrs -> do
      pure
        . UnfinishedBusiness_H
        $ attrs
        & (placementL .~ InThreatArea iid)
        & (removeAfterResolutionL .~ False)
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      hasCards <-
        fieldP
          InvestigatorHand
          ((>= 2) . length . filter (`cardMatch` NonWeakness))
          iid
      chooseOneM iid do
        when hasCards do
          labeled "Choose and discard 2 cards from your hand" do
            discardFromHand iid attrs DiscardChoose 2
        labeled "Flip this back over" $ flipOverBy iid (attrs.ability 1) attrs
      pure s
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      chooseBeginSkillTest sid iid (attrs.ability 2) attrs [#willpower, #combat] (Fixed 4)
      pure s
    PassedThisSkillTest _ (isAbilitySource attrs 2 -> True) -> do
      card <- genCard Enemies.heretic_G
      send $ format card <> " is \"banished\""
      addToVictory (toTarget attrs)
      pure s
    Flip _ _ (isTarget attrs -> True) -> do
      let heretic = lookupCard Enemies.heretic_G (toCardId attrs)
      removeStory attrs
      createEnemy_ heretic attrs.placement
      pure s
    _ -> UnfinishedBusiness_H <$> liftRunMessage msg attrs
