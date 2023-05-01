module Arkham.Treachery.Cards.OminousPortents
  ( ominousPortents
  , OminousPortents(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype OminousPortents = OminousPortents TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ominousPortents :: TreacheryCard OminousPortents
ominousPortents = treachery OminousPortents Cards.ominousPortents

instance RunMessage OminousPortents where
  runMessage msg t@(OminousPortents attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> OminousPortents <$> runMessage msg attrs
