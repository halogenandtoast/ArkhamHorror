module Arkham.Treachery.Cards.EyesInTheTrees
  ( eyesInTheTrees
  , EyesInTheTrees(..)
  )
where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype EyesInTheTrees = EyesInTheTrees TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eyesInTheTrees :: TreacheryCard EyesInTheTrees
eyesInTheTrees = treachery EyesInTheTrees Cards.eyesInTheTrees

instance RunMessage EyesInTheTrees where
  runMessage msg t@(EyesInTheTrees attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> EyesInTheTrees <$> liftRunMessage msg attrs
