module Arkham.Treachery.Cards.MeddlesomeFamiliar
  ( meddlesomeFamiliar
  , MeddlesomeFamiliar(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype MeddlesomeFamiliar = MeddlesomeFamiliar TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

meddlesomeFamiliar :: TreacheryCard MeddlesomeFamiliar
meddlesomeFamiliar = treachery MeddlesomeFamiliar Cards.meddlesomeFamiliar

instance RunMessage MeddlesomeFamiliar where
  runMessage msg t@(MeddlesomeFamiliar attrs) = case msg of
    Revelation _iid source | isSource attrs source -> pure t
    _ -> MeddlesomeFamiliar <$> runMessage msg attrs
