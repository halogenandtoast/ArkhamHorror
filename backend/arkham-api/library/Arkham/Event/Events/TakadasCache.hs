module Arkham.Event.Events.TakadasCache (takadasCache) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted

newtype TakadasCache = TakadasCache EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

takadasCache :: EventCard TakadasCache
takadasCache = event TakadasCache Cards.takadasCache

instance RunMessage TakadasCache where
  runMessage msg e@(TakadasCache attrs) = runQueueT $ case msg of
    PlayThisEvent _iid (is attrs -> True) -> do
      pure e
    _ -> TakadasCache <$> liftRunMessage msg attrs
