module Arkham.Homebrew.CircusExMortis.Locations.CrowdedRow_048 (crowdedRow_048) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype CrowdedRow_048 = CrowdedRow_048 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crowdedRow_048 :: LocationCard CrowdedRow_048
crowdedRow_048 = location CrowdedRow_048 Cards.crowdedRow_048 1 (Static 2)

instance HasAbilities CrowdedRow_048 where
  getAbilities (CrowdedRow_048 a) =
    extendRevealed1 a
      $ restricted a 1 (exists $ NearestEnemyToLocationFallback a.id NonEliteEnemy)
      $ forced
      $ SuccessfulInvestigationResult #after You (be a) (atLeast 3)

instance RunMessage CrowdedRow_048 where
  runMessage msg l@(CrowdedRow_048 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select $ NearestEnemyToLocation (toId attrs) NonEliteEnemy
      chooseTargetM iid enemies $ placeDoomOn (attrs.ability 1) 1
      pure l
    _ -> CrowdedRow_048 <$> liftRunMessage msg attrs
