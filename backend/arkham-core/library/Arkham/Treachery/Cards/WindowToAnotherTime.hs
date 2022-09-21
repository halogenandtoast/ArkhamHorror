module Arkham.Treachery.Cards.WindowToAnotherTime
  ( windowToAnotherTime
  , WindowToAnotherTime(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype WindowToAnotherTime = WindowToAnotherTime TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

windowToAnotherTime :: TreacheryCard WindowToAnotherTime
windowToAnotherTime = treachery WindowToAnotherTime Cards.windowToAnotherTime

instance RunMessage WindowToAnotherTime where
  runMessage msg t@(WindowToAnotherTime attrs) = case msg of
    Revelation _iid source | isSource attrs source -> pure t
    _ -> WindowToAnotherTime <$> runMessage msg attrs
