module Arkham.Treachery.Cards.ExtradimensionalVisions
  ( extradimensionalVisions
  , ExtradimensionalVisions(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype ExtradimensionalVisions = ExtradimensionalVisions TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

extradimensionalVisions :: TreacheryCard ExtradimensionalVisions
extradimensionalVisions =
  treachery ExtradimensionalVisions Cards.extradimensionalVisions

instance RunMessage ExtradimensionalVisions where
  runMessage msg t@(ExtradimensionalVisions attrs) = case msg of
    Revelation _iid source | isSource attrs source -> pure t
    _ -> ExtradimensionalVisions <$> runMessage msg attrs
