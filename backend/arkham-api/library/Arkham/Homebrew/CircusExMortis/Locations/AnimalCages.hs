module Arkham.Homebrew.CircusExMortis.Locations.AnimalCages (animalCages) where

import Arkham.Asset.Types (Field (AssetSealedChaosTokens))
import Arkham.ChaosToken.Types
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Investigator.Types (Field (InvestigatorSealedChaosTokens))
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Projection

newtype AnimalCages = AnimalCages LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

animalCages :: LocationCard AnimalCages
animalCages =
  location AnimalCages Cards.animalCages 5 (PerPlayer 2)

instance HasModifiersFor AnimalCages where
  getModifiersFor (AnimalCages a) = do
    -- "moon tokens sealed on player cards at its location" = sealed MoonToken across
    -- investigators AND assets here. No combined helper exists, so count locally.
    investigators <- select $ investigatorAt (toId a)
    assets <- select $ assetAt (toId a)
    investigatorMoons <-
      sum <$> for investigators \iid -> countMoons <$> field InvestigatorSealedChaosTokens iid
    assetMoons <-
      sum <$> for assets \aid -> countMoons <$> field AssetSealedChaosTokens aid
    let n = investigatorMoons + assetMoons
    -- ponytail: cap the reduction at (printed shroud 5 - 1) to honor "minimum of 1
    -- shroud"; matches ParlorTheMidwinterGala's approach (engine has no shroud floor).
    when (n > 0) $ modifySelf a [ShroudModifier (negate (min n 4))]
   where
    countMoons = length . filter ((== MoonToken) . (.face))

instance RunMessage AnimalCages where
  runMessage msg (AnimalCages attrs) = runQueueT $ case msg of
    _ -> AnimalCages <$> liftRunMessage msg attrs
