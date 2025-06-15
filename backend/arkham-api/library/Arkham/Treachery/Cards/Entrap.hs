module Arkham.Treachery.Cards.Entrap (entrap) where

import Arkham.Matcher
import Arkham.Message.Lifted
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies

newtype Entrap = Entrap TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

entrap :: TreacheryCard Entrap
entrap = treachery Entrap Cards.entrap

instance RunMessage Entrap where
  runMessage msg t@(Entrap attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      mEnemy <- selectOne $ enemyIs Enemies.theBloodlessMan
      case mEnemy of
        Nothing -> gainSurge attrs
        Just eid -> do
          withLocationOf eid $ \eloc -> withLocationOf iid $ \iloc -> do
            if eloc == iloc
              then pure () -- TODO: make a Guest asset spellbound
              else do
                ready eid
                moveUntil iloc (EnemyTarget eid)
                enemyEngageInvestigator eid iid
      pure t
    _ -> Entrap <$> liftRunMessage msg attrs
