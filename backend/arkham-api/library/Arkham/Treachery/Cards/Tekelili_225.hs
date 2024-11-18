module Arkham.Treachery.Cards.Tekelili_225
  ( tekelili_225
  , Tekelili_225(..)
  )
where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Tekelili_225 = Tekelili_225 TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tekelili_225 :: TreacheryCard Tekelili_225
tekelili_225 = treachery Tekelili_225 Cards.tekelili_225

instance RunMessage Tekelili_225 where
  runMessage msg t@(Tekelili_225 attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> Tekelili_225 <$> liftRunMessage msg attrs
