module Arkham.Treachery.Cards.GraveLight
  ( graveLight
  , GraveLight(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype GraveLight = GraveLight TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

graveLight :: TreacheryCard GraveLight
graveLight = treachery GraveLight Cards.graveLight

instance RunMessage GraveLight where
  runMessage msg t@(GraveLight attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> GraveLight <$> runMessage msg attrs
