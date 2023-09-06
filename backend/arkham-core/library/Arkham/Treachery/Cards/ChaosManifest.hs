module Arkham.Treachery.Cards.ChaosManifest
  ( chaosManifest
  , ChaosManifest(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype ChaosManifest = ChaosManifest TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chaosManifest :: TreacheryCard ChaosManifest
chaosManifest = treachery ChaosManifest Cards.chaosManifest

instance RunMessage ChaosManifest where
  runMessage msg t@(ChaosManifest attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> ChaosManifest <$> runMessage msg attrs
