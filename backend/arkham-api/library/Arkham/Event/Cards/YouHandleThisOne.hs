module Arkham.Event.Cards.YouHandleThisOne (youHandleThisOne, YouHandleThisOne (..)) where

import Arkham.Classes.HasQueue
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher

newtype YouHandleThisOne = YouHandleThisOne EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

youHandleThisOne :: EventCard YouHandleThisOne
youHandleThisOne = event YouHandleThisOne Cards.youHandleThisOne

instance RunMessage YouHandleThisOne where
  runMessage msg e@(YouHandleThisOne attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      selectOrRunOneToHandle iid attrs $ not_ (InvestigatorWithId iid)
      gainResourcesIfCan iid attrs 1
      pure e
    HandleTargetChoice iid (isSource attrs -> True) (InvestigatorTarget iid') -> do
      changeDrawnBy iid iid'
      pure e
    _ -> YouHandleThisOne <$> liftRunMessage msg attrs
