module Arkham.Homebrew.CircusExMortis.Locations.AnimalCages (animalCages) where

import Arkham.Asset.Types (Field (AssetSealedChaosTokens))
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.CircusExMortis.Tokens (pattern MoonToken)
import Arkham.Investigator.Types (Field (InvestigatorSealedChaosTokens))
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype AnimalCages = AnimalCages LocationAttrs
  deriving anyclass (IsLocation, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

animalCages :: LocationCard AnimalCages
animalCages = location AnimalCages Cards.animalCages 5 (PerPlayer 2)

instance HasModifiersFor AnimalCages where
  getModifiersFor (AnimalCages a) = do
    -- "moon tokens sealed on player cards at its location" = sealed MoonToken across
    -- investigators AND assets here. No combined helper exists, so count locally.
    investigatorMoons <- selectSumWith countMoons InvestigatorSealedChaosTokens $ investigatorAt a.id
    assetMoons <- selectSumWith countMoons AssetSealedChaosTokens $ assetAt (toId a)
    let n = investigatorMoons + assetMoons
    when (n > 0) $ modifySelf a [ShroudModifier $ negate $ min n 4]
   where
    countMoons = count ((== MoonToken) . (.face))
