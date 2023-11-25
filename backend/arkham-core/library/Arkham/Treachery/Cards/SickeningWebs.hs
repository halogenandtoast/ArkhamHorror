module Arkham.Treachery.Cards.SickeningWebs (
  sickeningWebs,
  SickeningWebs (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype SickeningWebs = SickeningWebs TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sickeningWebs :: TreacheryCard SickeningWebs
sickeningWebs = treachery SickeningWebs Cards.sickeningWebs

instance RunMessage SickeningWebs where
  runMessage msg t@(SickeningWebs attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> SickeningWebs <$> runMessage msg attrs
