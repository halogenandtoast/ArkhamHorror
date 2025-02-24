module Arkham.Skill.Cards.QuickWitted1 (quickWitted1) where

import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Data.List qualified as L

newtype QuickWitted1 = QuickWitted1 SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

quickWitted1 :: SkillCard QuickWitted1
quickWitted1 = skill QuickWitted1 Cards.quickWitted1

instance HasModifiersFor QuickWitted1 where
  getModifiersFor (QuickWitted1 attrs) = do
    n <- selectCount
      $ inDiscardOf attrs.owner
      <> basic (cardIs Cards.quickWitted1)
    when (n > 0) do
      modified_ attrs attrs.cardId [AddSkillIcons $ concat $ L.replicate n [#intellect, #agility]]

instance RunMessage QuickWitted1 where
  runMessage msg s@(QuickWitted1 attrs) = 
    let
      handleIt = do
        others <- select
          $ inDiscardOf attrs.owner
          <> basic (cardIs Cards.quickWitted1 <> not_ (CardWithId attrs.cardId))
        unless (null others) do
          chooseOneM attrs.owner do
            labeled "Shuffle each other Quick-Witted in your discard pile into your deck" $ shuffleCardsIntoDeck attrs.owner others
            labeled "Do not shuffle any" nothing
        pure s
    in runQueueT $ case msg of
      PassedSkillTest _ _ _ (isTarget attrs -> True) _ _ -> handleIt
      FailedSkillTest _ _ _ (isTarget attrs -> True) _ _ -> handleIt
      _ -> QuickWitted1 <$> liftRunMessage msg attrs
