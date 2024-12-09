module Arkham.Treachery.Cards.Indebted (Indebted (..), indebted) where

import Arkham.Classes
import Arkham.Modifier
import Arkham.Placement
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

newtype Indebted = Indebted TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

indebted :: TreacheryCard Indebted
indebted = treachery Indebted Cards.indebted

instance HasModifiersFor Indebted where
  getModifiersFor (Indebted attrs) = case attrs.placement of
    InThreatArea iid ->
      modifiedWith_ attrs iid setActiveDuringSetup [StartingResources (-2)]
    _ -> pure mempty

instance RunMessage Indebted where
  runMessage msg t@(Indebted attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ placeInThreatArea attrs iid
      pure t
    _ -> Indebted <$> runMessage msg attrs
