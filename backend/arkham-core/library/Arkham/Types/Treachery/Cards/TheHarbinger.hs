module Arkham.Types.Treachery.Cards.TheHarbinger
  ( theHarbinger
  , TheHarbinger(..)
  ) where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype TheHarbinger = TheHarbinger TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHarbinger :: TreacheryCard TheHarbinger
theHarbinger = treachery TheHarbinger Cards.theHarbinger

instance TreacheryRunner env => RunMessage env TheHarbinger where
  runMessage msg t@(TheHarbinger attrs) = case msg of
    Revelation iid source | isSource attrs source -> pure t
    _ -> TheHarbinger <$> runMessage msg attrs
