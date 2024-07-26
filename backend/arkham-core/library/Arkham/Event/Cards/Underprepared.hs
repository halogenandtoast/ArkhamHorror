module Arkham.Event.Cards.Underprepared (underprepared, Underprepared (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Placement

newtype Underprepared = Underprepared EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

underprepared :: EventCard Underprepared
underprepared = event Underprepared Cards.underprepared

instance HasModifiersFor Underprepared where
  getModifiersFor (InvestigatorTarget iid) (Underprepared attrs) = do
    case attrs.placement of
      StillInHand iid' | iid == iid' -> modified attrs [FewerMatchingIconsPerCard 1]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage Underprepared where
  runMessage msg e@(Underprepared attrs) = runQueueT $ case msg of
    PlayThisEvent _iid (is attrs -> True) -> do
      pure e
    _ -> Underprepared <$> liftRunMessage msg attrs
