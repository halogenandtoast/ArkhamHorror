module Arkham.Skill.Cards.MemoriesOfAnotherLife5 (memoriesOfAnotherLife5) where

import Arkham.Card
import Arkham.Cost.Status
import Arkham.Helpers.Playable
import Arkham.Message.Lifted.Choose
import Arkham.PlayerCard
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted hiding (PaidCost)
import Arkham.Window (defaultWindows)

newtype MemoriesOfAnotherLife5 = MemoriesOfAnotherLife5 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

memoriesOfAnotherLife5 :: SkillCard MemoriesOfAnotherLife5
memoriesOfAnotherLife5 = skill MemoriesOfAnotherLife5 Cards.memoriesOfAnotherLife5

instance RunMessage MemoriesOfAnotherLife5 where
  runMessage msg s@(MemoriesOfAnotherLife5 attrs) = runQueueT $ case msg of
    PassedSkillTest _ _ _ (isTarget attrs -> True) _ _ -> do
      chooseOneM attrs.owner do
        labeled "Remove Memories of Another Life from the game" (doStep 1 msg)
        labeled "Do not remove Memories of Another Lift" nothing
      pure s
    DoStep 1 (PassedSkillTest _ _ _ (isTarget attrs -> True) _ _) -> do
      let iid = attrs.owner
      let filterIt def =
            (def.kind `elem` [AssetType, EventType])
              && (def.level == Just 0)
              && not def.permanent
              && isJust def.cost
      let cards =
            filter filterIt
              $ mapMaybe lookupCardDef
              $ toList allPlayerCards

      chooseOneM attrs.owner do
        for_ cards \def -> do
          card <- genCard def
          playable <- getIsPlayable iid attrs PaidCost (defaultWindows iid) card
          removeCard card.id
          when playable do
            cardLabeled card $ handleTarget iid attrs (CardCodeTarget card.cardCode)
      pure s
    HandleTargetChoice iid (isSource attrs -> True) (CardCodeTarget cardCode) -> do
      removeFromGame attrs
      for_ (lookupCardDef cardCode) \def -> do
        card <- genCard def
        putCardIntoPlay iid card
      pure s
    _ -> MemoriesOfAnotherLife5 <$> liftRunMessage msg attrs
