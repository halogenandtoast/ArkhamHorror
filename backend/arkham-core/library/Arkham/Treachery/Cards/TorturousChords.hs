module Arkham.Treachery.Cards.TorturousChords
  ( torturousChords
  , TorturousChords(..)
  ) where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Attrs
import Arkham.Treachery.Runner

newtype TorturousChords = TorturousChords TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

torturousChords :: TreacheryCard TorturousChords
torturousChords = treachery TorturousChords Cards.torturousChords

instance TreacheryRunner env => RunMessage env TorturousChords where
  runMessage msg t@(TorturousChords attrs) = case msg of
    Revelation _iid source | isSource attrs source ->
      t <$ push (Discard $ toTarget attrs)
    _ -> TorturousChords <$> runMessage msg attrs
