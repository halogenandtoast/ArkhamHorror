module Arkham.Treachery.Cards.DisquietingDreams
  ( disquietingDreams
  , DisquietingDreams(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype DisquietingDreams = DisquietingDreams TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

disquietingDreams :: TreacheryCard DisquietingDreams
disquietingDreams = treachery DisquietingDreams Cards.disquietingDreams

instance RunMessage DisquietingDreams where
  runMessage msg t@(DisquietingDreams attrs) = case msg of
    Revelation _iid source | isSource attrs source -> pure t
    _ -> DisquietingDreams <$> runMessage msg attrs
