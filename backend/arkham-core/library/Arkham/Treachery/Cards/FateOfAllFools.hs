module Arkham.Treachery.Cards.FateOfAllFools
  ( fateOfAllFools
  , FateOfAllFools(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype FateOfAllFools = FateOfAllFools TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fateOfAllFools :: TreacheryCard FateOfAllFools
fateOfAllFools = treachery FateOfAllFools Cards.fateOfAllFools

instance RunMessage FateOfAllFools where
  runMessage msg t@(FateOfAllFools attrs) = case msg of
    Revelation _iid source | isSource attrs source -> pure t
    _ -> FateOfAllFools <$> runMessage msg attrs
