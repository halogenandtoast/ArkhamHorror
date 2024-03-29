module Arkham.Treachery.Cards.SirenCall
  ( sirenCall
  , SirenCall(..)
  )
where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype SirenCall = SirenCall TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sirenCall :: TreacheryCard SirenCall
sirenCall = treachery SirenCall Cards.sirenCall

instance RunMessage SirenCall where
  runMessage msg t@(SirenCall attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> SirenCall <$> lift (runMessage msg attrs)
