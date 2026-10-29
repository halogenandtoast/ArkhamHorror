module Arkham.Skill.Cards.MemoriesOfAnotherLife5 (memoriesOfAnotherLife5) where

import Arkham.Card
import Arkham.Cost.Status
import Arkham.Helpers.Playable
import Arkham.I18n
import Arkham.Message.Lifted.Choose
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
      skillTestCardOption attrs do
        chooseOneM attrs.owner $ cardI18n $ scope "memoriesOfAnotherLife5" do
          labeled "removeFromGame" (doStep 1 msg)
          labeled "doNotRemove" nothing
      pure s
    DoStep 1 (PassedSkillTest _ _ _ (isTarget attrs -> True) _ _) -> do
      let iid = attrs.owner
      let filterIt def =
            (def.kind `elem` [AssetType, EventType])
              && (def.level == Just 0)
              && not def.permanent
              && isJust def.cost
      cards <- filter filterIt <$> collectionCardDefs

      playable <-
        filterM
          ( \def -> do
              card <- genCard def
              canPlay <- getIsPlayable iid attrs PaidCost (defaultWindows iid) card
              removeCard card.id
              pure canPlay
          )
          cards
      chooseCollectionCards iid attrs playable
      pure s
    HandleTargetChoice iid (isSource attrs -> True) (CardCodeTarget cardCode) -> do
      removeFromGame attrs
      for_ (lookupCardDef cardCode) \def -> do
        card <- genCard def
        putCardIntoPlay iid card
      pure s
    _ -> MemoriesOfAnotherLife5 <$> liftRunMessage msg attrs
