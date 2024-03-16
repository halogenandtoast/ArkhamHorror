module Arkham.Treachery.Cards.DholeTunnel
  ( dholeTunnel
  , DholeTunnel(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype DholeTunnel = DholeTunnel TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dholeTunnel :: TreacheryCard DholeTunnel
dholeTunnel = treachery DholeTunnel Cards.dholeTunnel

instance RunMessage DholeTunnel where
  runMessage msg t@(DholeTunnel attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> DholeTunnel <$> runMessage msg attrs
