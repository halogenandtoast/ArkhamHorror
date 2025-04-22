module Arkham.Treachery.Cards.CaughtRedHanded (caughtRedHanded) where

import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CaughtRedHanded = CaughtRedHanded TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

caughtRedHanded :: TreacheryCard CaughtRedHanded
caughtRedHanded = treachery CaughtRedHanded Cards.caughtRedHanded

instance RunMessage CaughtRedHanded where
  runMessage msg t@(CaughtRedHanded attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      enemies <- selectTargets $ EnemyAt $ orConnected $ locationWithInvestigator iid
      hunters <- selectTargets $ HunterEnemy <> at_ (ConnectedFrom $ locationWithInvestigator iid)
      for_ enemies readyThis
      for_ hunters (`moveToward` locationWithInvestigator iid)
      when (null hunters) $ shuffleIntoDeck iid attrs
      pure t
    _ -> CaughtRedHanded <$> liftRunMessage msg attrs
