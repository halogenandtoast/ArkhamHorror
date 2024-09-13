module Arkham.Treachery.Cards.FracturedConsciousness
  ( fracturedConsciousness
  , FracturedConsciousness(..)
  )
where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype FracturedConsciousness = FracturedConsciousness TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fracturedConsciousness :: TreacheryCard FracturedConsciousness
fracturedConsciousness = treachery FracturedConsciousness Cards.fracturedConsciousness

instance RunMessage FracturedConsciousness where
  runMessage msg t@(FracturedConsciousness attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> FracturedConsciousness <$> liftRunMessage msg attrs
