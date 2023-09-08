module Arkham.Treachery.Cards.UltimateChaos
  ( ultimateChaos
  , UltimateChaos(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype UltimateChaos = UltimateChaos TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ultimateChaos :: TreacheryCard UltimateChaos
ultimateChaos = treachery UltimateChaos Cards.ultimateChaos

instance RunMessage UltimateChaos where
  runMessage msg t@(UltimateChaos attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> UltimateChaos <$> runMessage msg attrs
