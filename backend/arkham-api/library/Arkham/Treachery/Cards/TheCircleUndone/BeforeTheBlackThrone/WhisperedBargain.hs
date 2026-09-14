module Arkham.Treachery.Cards.TheCircleUndone.BeforeTheBlackThrone.WhisperedBargain (whisperedBargain) where

import Arkham.Enemy.CardDefs.TheCircleUndone.BeforeTheBlackThrone qualified as Enemies
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheCircleUndone.BeforeTheBlackThrone.Helpers
import Arkham.Treachery.CardDefs.TheCircleUndone.BeforeTheBlackThrone qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype WhisperedBargain = WhisperedBargain TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whisperedBargain :: TreacheryCard WhisperedBargain
whisperedBargain = treachery WhisperedBargain Cards.whisperedBargain

instance RunMessage WhisperedBargain where
  runMessage msg t@(WhisperedBargain attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      azathoth <- selectJust $ IncludeOmnipotent $ enemyIs Enemies.azathoth
      chooseOneM iid $ scenarioI18n do
        labeled "whisperedBargain.doom" $ placeDoom attrs azathoth 1
        labeled "whisperedBargain.attack" $ initiateEnemyAttack azathoth attrs iid
      pure t
    _ -> WhisperedBargain <$> liftRunMessage msg attrs
