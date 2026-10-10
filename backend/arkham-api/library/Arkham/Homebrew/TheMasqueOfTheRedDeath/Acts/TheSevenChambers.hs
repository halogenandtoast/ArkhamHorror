module Arkham.Homebrew.TheMasqueOfTheRedDeath.Acts.TheSevenChambers (theSevenChambers) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Location (unrevealLocation)
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Locations qualified as Locations
import Arkham.Matcher
import Arkham.Trait (Trait (Guest))

newtype TheSevenChambers = TheSevenChambers ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSevenChambers :: ActCard TheSevenChambers
theSevenChambers = act (1, A) TheSevenChambers Cards.theSevenChambers Nothing

instance HasAbilities TheSevenChambers where
  -- "Objective - At the end of the round, if each undefeated investigator is at
  -- Black Chamber, advance."
  getAbilities = actAbilities1 \a ->
    restricted a 1 (EachUndefeatedInvestigator $ at_ $ locationIs Locations.blackChamber)
      $ Objective
      $ forced (RoundEnds #when)

instance RunMessage TheSevenChambers where
  runMessage msg a@(TheSevenChambers attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      lead <- getLead
      createSetAsideEnemy_ Enemies.theRedDeath (locationIs Locations.blackChamber)
      -- Orange Chamber and Black Chamber are the only locations with Victory X, so
      -- they keep whatever is left on them.
      selectEach (not_ LocationWithVictory) (placeCluesUpToClueValue attrs)
      -- Each asset owns what its own back is; the act only turns them over.
      let hosts = oneOf [assetIs Assets.prosperoPrinceGregariousHost, AssetWithTrait Guest]
      selectEach hosts (flipOverBy lead attrs)
      selectEach (locationIs Locations.grandBallroom) unrevealLocation
      advanceActDeck attrs
      pure a
    _ -> TheSevenChambers <$> liftRunMessage msg attrs
