module Arkham.Location.Cards.GrandBazaarCrowdedShops (grandBazaarCrowdedShops) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype GrandBazaarCrowdedShops = GrandBazaarCrowdedShops LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grandBazaarCrowdedShops :: LocationCard GrandBazaarCrowdedShops
grandBazaarCrowdedShops =
  locationWith
    GrandBazaarCrowdedShops
    Cards.grandBazaarCrowdedShops
    3
    (PerPlayer 1)
    connectsToAdjacent

instance HasAbilities GrandBazaarCrowdedShops where
  getAbilities (GrandBazaarCrowdedShops a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> thisExists a LocationWithAnyClues <> exists (enemy_ #cultist))
      $ forced
      $ CampaignEvent #after (Just You) "exposed[decoy]"

instance RunMessage GrandBazaarCrowdedShops where
  runMessage msg l@(GrandBazaarCrowdedShops attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      cultists <- select $ NearestEnemyToFallback iid #cultist
      chooseTargetM iid cultists $ moveTokensTo (attrs.ability 1) attrs #clue 1
      pure l
    _ -> GrandBazaarCrowdedShops <$> liftRunMessage msg attrs
