module Arkham.Treachery.Cards.DeepOneInvasion
  ( deepOneInvasion
  , DeepOneInvasion(..)
  )
where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DeepOneInvasion = DeepOneInvasion TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deepOneInvasion :: TreacheryCard DeepOneInvasion
deepOneInvasion = treachery DeepOneInvasion Cards.deepOneInvasion

instance RunMessage DeepOneInvasion where
  runMessage msg t@(DeepOneInvasion attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> DeepOneInvasion <$> liftRunMessage msg attrs
