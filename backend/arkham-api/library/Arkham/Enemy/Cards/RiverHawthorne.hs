module Arkham.Enemy.Cards.RiverHawthorne (riverHawthorne) where

import Arkham.Ability
import Arkham.Card (toCardId, toCardType)
import Arkham.Card.CardType
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Matcher
import Arkham.Message.Lifted.Placement
import Arkham.Treachery.Import.Lifted (gainSurge)

newtype RiverHawthorne = RiverHawthorne EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riverHawthorne :: EnemyCard RiverHawthorne
riverHawthorne = enemy RiverHawthorne Cards.riverHawthorne

instance HasAbilities RiverHawthorne where
  getAbilities (RiverHawthorne a) =
    extend1 a $ skillTestAbility $ restricted a 1 OnSameLocation parleyAction_

instance RunMessage RiverHawthorne where
  runMessage msg e@(RiverHawthorne attrs) = runQueueT $ case msg of
    BeginRound -> pure $ RiverHawthorne $ attrs & setMeta True
    InvestigatorDrewEncounterCard iid ec | getEnemyMetaDefault True attrs -> do
      atRiver <- iid <=~> InvestigatorAt (locationWithEnemy attrs)
      if atRiver && toCardType ec == TreacheryType
        then do
          push $ GainSurge (attrs.ability 1) (CardIdTarget $ toCardId ec)
          pure $ RiverHawthorne $ attrs & setMeta False
        else pure e
    Revelation iid (TreacherySource tid) | getEnemyMetaDefault True attrs -> do
      atRiver <- iid <=~> InvestigatorAt (locationWithEnemy attrs)
      if atRiver
        then do
          priority $ gainSurge tid
          pure $ RiverHawthorne $ attrs & setMeta False
        else pure e
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) attrs #willpower (Fixed 5)
      pure e
    PassedThisSkillTest _iid (isAbilitySource attrs 1 -> True) -> do
      place attrs (OutOfPlay SetAsideZone)
      pure e
    _ -> RiverHawthorne <$> liftRunMessage msg attrs
