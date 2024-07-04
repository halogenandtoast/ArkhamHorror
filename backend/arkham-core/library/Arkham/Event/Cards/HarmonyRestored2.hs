module Arkham.Event.Cards.HarmonyRestored2 (harmonyRestored2, HarmonyRestored2 (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher

newtype HarmonyRestored2 = HarmonyRestored2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

harmonyRestored2 :: EventCard HarmonyRestored2
harmonyRestored2 = event HarmonyRestored2 Cards.harmonyRestored2

instance RunMessage HarmonyRestored2 where
  runMessage msg e@(HarmonyRestored2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      n <- selectCount $ ChaosTokenFaceIs #bless
      x <- take n <$> select (ChaosTokenFaceIs #curse)

      push $ ReturnChaosTokensToPool x
      gainResourcesIfCan iid attrs (length x)
      pure e
    _ -> HarmonyRestored2 <$> liftRunMessage msg attrs
