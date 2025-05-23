module Arkham.Treachery.Cards.Indebted (indebted) where

import Arkham.Helpers.Modifiers (modifiedWith_)
import Arkham.Modifier
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

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
  runMessage msg t@(Indebted attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    _ -> Indebted <$> liftRunMessage msg attrs
