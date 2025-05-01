module Arkham.Treachery.Cards.ArousingSuspicions (arousingSuspicions) where

import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ArousingSuspicions = ArousingSuspicions TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arousingSuspicions :: TreacheryCard ArousingSuspicions
arousingSuspicions = treachery ArousingSuspicions Cards.arousingSuspicions

instance RunMessage ArousingSuspicions where
  runMessage msg t@(ArousingSuspicions attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      criminals <- select $ enemyAtLocationWith iid <> #criminal <> CanPlaceDoomOnEnemy
      if null criminals
        then loseResources iid attrs 2
        else for_ criminals \eid -> placeDoom attrs eid 1
      pure t
    _ -> ArousingSuspicions <$> liftRunMessage msg attrs
