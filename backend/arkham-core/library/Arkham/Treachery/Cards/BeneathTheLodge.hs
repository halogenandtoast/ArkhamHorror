module Arkham.Treachery.Cards.BeneathTheLodge
  ( beneathTheLodge
  , BeneathTheLodge(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype BeneathTheLodge = BeneathTheLodge TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beneathTheLodge :: TreacheryCard BeneathTheLodge
beneathTheLodge = treachery BeneathTheLodge Cards.beneathTheLodge

instance RunMessage BeneathTheLodge where
  runMessage msg t@(BeneathTheLodge attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> BeneathTheLodge <$> runMessage msg attrs
