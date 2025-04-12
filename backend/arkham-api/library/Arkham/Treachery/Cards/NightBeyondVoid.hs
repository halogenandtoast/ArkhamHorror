module Arkham.Treachery.Cards.NightBeyondVoid (nightBeyondVoid) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted
import Arkham.Matcher

newtype NightBeyondVoid = NightBeyondVoid TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nightBeyondVoid :: TreacheryCard NightBeyondVoid
nightBeyondVoid = treachery NightBeyondVoid Cards.nightBeyondVoid

instance RunMessage NightBeyondVoid where
  runMessage msg t@(NightBeyondVoid attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      mShadowSpawned <- selectOne $ treacheryIs Cards.shadowSpawned
      for_ mShadowSpawned \shadowSpawned -> placeTokens attrs shadowSpawned #resource 1
      pure t
    _ -> NightBeyondVoid <$> liftRunMessage msg attrs
