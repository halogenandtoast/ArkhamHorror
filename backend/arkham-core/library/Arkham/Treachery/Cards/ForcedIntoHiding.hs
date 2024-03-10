module Arkham.Treachery.Cards.ForcedIntoHiding
  ( forcedIntoHiding
  , ForcedIntoHiding(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype ForcedIntoHiding = ForcedIntoHiding TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forcedIntoHiding :: TreacheryCard ForcedIntoHiding
forcedIntoHiding = treachery ForcedIntoHiding Cards.forcedIntoHiding

instance RunMessage ForcedIntoHiding where
  runMessage msg t@(ForcedIntoHiding attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> ForcedIntoHiding <$> runMessage msg attrs
