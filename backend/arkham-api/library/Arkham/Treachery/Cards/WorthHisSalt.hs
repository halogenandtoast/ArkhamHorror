module Arkham.Treachery.Cards.WorthHisSalt
  ( worthHisSalt
  , WorthHisSalt(..)
  )
where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype WorthHisSalt = WorthHisSalt TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

worthHisSalt :: TreacheryCard WorthHisSalt
worthHisSalt = treachery WorthHisSalt Cards.worthHisSalt

instance RunMessage WorthHisSalt where
  runMessage msg t@(WorthHisSalt attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> WorthHisSalt <$> liftRunMessage msg attrs
