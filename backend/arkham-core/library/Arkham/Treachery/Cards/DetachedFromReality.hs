module Arkham.Treachery.Cards.DetachedFromReality (
  detachedFromReality,
  DetachedFromReality (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype DetachedFromReality = DetachedFromReality TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

detachedFromReality :: TreacheryCard DetachedFromReality
detachedFromReality = treachery DetachedFromReality Cards.detachedFromReality

instance RunMessage DetachedFromReality where
  runMessage msg t@(DetachedFromReality attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> DetachedFromReality <$> runMessage msg attrs
