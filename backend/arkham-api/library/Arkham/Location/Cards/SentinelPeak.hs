module Arkham.Location.Cards.SentinelPeak (sentinelPeak) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (sentinelPeak)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait

newtype SentinelPeak = SentinelPeak LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sentinelPeak :: LocationCard SentinelPeak
sentinelPeak =
  locationWith SentinelPeak Cards.sentinelPeak 4 (PerPlayer 2)
    $ costToEnterUnrevealedL
    .~ GroupClueCost (PerPlayer 2) Anywhere

instance HasAbilities SentinelPeak where
  getAbilities (SentinelPeak a) =
    extendRevealed1 a
      $ restricted a 1 Here
      $ forced
      $ DrawCard #when You (basic $ CardWithTrait Hex) AnyDeck

instance RunMessage SentinelPeak where
  runMessage msg l@(SentinelPeak attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid attrs 1
      pure l
    _ -> SentinelPeak <$> liftRunMessage msg attrs
