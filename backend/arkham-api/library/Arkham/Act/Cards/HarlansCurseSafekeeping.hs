module Arkham.Act.Cards.HarlansCurseSafekeeping (harlansCurseSafekeeping) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.GameValue
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Placement
import Arkham.Scenarios.ThreadsOfFate.Helpers

newtype HarlansCurseSafekeeping = HarlansCurseSafekeeping ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

harlansCurseSafekeeping :: ActCard HarlansCurseSafekeeping
harlansCurseSafekeeping = act (2, A) HarlansCurseSafekeeping Cards.harlansCurseSafekeeping Nothing

instance HasAbilities HarlansCurseSafekeeping where
  getAbilities = actAbilities1 \a ->
    restricted a 1 (exists $ assetIs Assets.harlanEarnstone <> AssetWithClues (AtLeast $ PerPlayer 1))
      $ Objective
      $ forced AnyWindow

instance RunMessage HarlansCurseSafekeeping where
  runMessage msg a@(HarlansCurseSafekeeping attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      n <- perPlayer 1
      selectEach (assetIs Assets.harlanEarnstone) (removeCluesFrom (attrs.ability 1) n)
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      lead <- getLead
      relicOfAges <- getSetAsideCard Assets.relicOfAgesADeviceOfSomeSort
      curiositieShoppe <- selectJust $ locationIs Locations.curiositieShoppe
      createAssetAt_ relicOfAges (AttachedToLocation curiositieShoppe)
      deckCount <- getActDecksInPlayCount
      acolyteCount <- if deckCount <= 2 then getPlayerCountValue (ByPlayerCount 1 1 2 2) else pure 0
      repeated acolyteCount $ findEncounterCardIn lead attrs (cardIs Enemies.acolyte) [#deck]
      advanceToAct attrs Cards.findTheRelic A
      pure a
    FoundEncounterCard _ (isTarget attrs -> True) card -> do
      curiositieShoppe <- selectJust $ locationIs Locations.curiositieShoppe
      spawnEnemyAt_ card curiositieShoppe
      pure a
    _ -> HarlansCurseSafekeeping <$> liftRunMessage msg attrs
