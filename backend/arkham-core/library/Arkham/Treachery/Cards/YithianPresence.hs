module Arkham.Treachery.Cards.YithianPresence
  ( yithianPresence
  , YithianPresence(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype YithianPresence = YithianPresence TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yithianPresence :: TreacheryCard YithianPresence
yithianPresence = treachery YithianPresence Cards.yithianPresence

instance RunMessage YithianPresence where
  runMessage msg t@(YithianPresence attrs) = case msg of
    Revelation _iid source | isSource attrs source -> pure t
    _ -> YithianPresence <$> runMessage msg attrs
