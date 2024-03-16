module Arkham.Treachery.Cards.LitByDeathFire
  ( litByDeathFire
  , LitByDeathFire(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype LitByDeathFire = LitByDeathFire TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

litByDeathFire :: TreacheryCard LitByDeathFire
litByDeathFire = treachery LitByDeathFire Cards.litByDeathFire

instance RunMessage LitByDeathFire where
  runMessage msg t@(LitByDeathFire attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> LitByDeathFire <$> runMessage msg attrs
