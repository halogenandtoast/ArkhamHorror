module Arkham.Act.Cards.CarefulNavigation (carefulNavigation) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheDrownedCity.Key (TheDrownedCityKey (DiscoveredGlyphs))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Trait (Trait (Vault))

newtype CarefulNavigation = CarefulNavigation ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

carefulNavigation :: ActCard CarefulNavigation
carefulNavigation = act (1, A) CarefulNavigation Cards.carefulNavigation Nothing

instance HasAbilities CarefulNavigation where
  getAbilities (CarefulNavigation a) =
    extend
      a
      [ -- "After the investigators have translated any 3 of the following glyphs
        -- (L, M, N, O, P): Reveal the Sealed Chamber location." All five live in
        -- this scenario: L and M on the two Chamber of Records locations, N, O, and
        -- P on the three Ancient Vault treacheries. The campaign records each
        -- translated glyph's letter into the DiscoveredGlyphs set and fires the
        -- "translateGlyph" window afterwards, so the reaction below sees the glyph
        -- that just landed.
        restricted a 1 (exists (sealedChamber <> UnrevealedLocation) <> translatedThreeGlyphs)
          $ forced (CampaignEvent #after Nothing "translateGlyph")
      , -- (Objective) When the round ends, if an investigator controls the Tidal
        -- Tablet story asset, advance.
        restricted a 2 (exists $ assetIs Assets.tidalTablet <> AssetControlledBy Anyone)
          $ Objective
          $ forced (RoundEnds #when)
      ]

instance RunMessage CarefulNavigation where
  runMessage msg a@(CarefulNavigation attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      -- Sealed Chamber is the unrevealed side of Chamber of the Tablet, which setup
      -- already put into play at the far right of the grid; revealing it flips the
      -- location to its (Unsealed) side.
      selectEach (sealedChamber <> UnrevealedLocation) reveal
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      -- Increase the flood level of each location that can have its flood level
      -- increased.
      selectEach CanHaveFloodLevelIncreased (push . IncreaseFloodLevel)
      -- Place clues on each revealed Vault location without victory, up to its
      -- clue threshold.
      selectEach (RevealedLocation <> LocationWithTrait Vault <> not_ LocationWithVictory)
        $ placeCluesUpToClueValue attrs
      advanceActDeck attrs
      pure a
    _ -> CarefulNavigation <$> liftRunMessage msg attrs

-- | The Chamber of the Tablet's unrevealed side, which setup puts into play.
sealedChamber :: LocationMatcher
sealedChamber = locationIs Locations.chamberOfTheTabletUnsealed

-- | Any 3 of the five glyphs this scenario can translate.
translatedThreeGlyphs :: Criterion
translatedThreeGlyphs = recordSetHasAtLeast (Static 3) DiscoveredGlyphs ["L", "M", "N", "O", "P"]
