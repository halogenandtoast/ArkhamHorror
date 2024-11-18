module Arkham.Treachery.Cards.Tekelili_227
  ( tekelili_227
  , Tekelili_227(..)
  )
where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Tekelili_227 = Tekelili_227 TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tekelili_227 :: TreacheryCard Tekelili_227
tekelili_227 = treachery Tekelili_227 Cards.tekelili_227

instance RunMessage Tekelili_227 where
  runMessage msg t@(Tekelili_227 attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> Tekelili_227 <$> liftRunMessage msg attrs
