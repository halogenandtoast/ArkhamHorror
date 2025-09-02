module Arkham.Act.Cards.ACosmicJourney (aCosmicJourney) where

import Arkham.Ability hiding (Cosmos)
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Uses
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Scenario
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Scenario.Deck
import Arkham.Scenarios.FilmFatale.Helpers (initialLayout)
import Arkham.Trait (Trait (Cosmos, RitualSite))

newtype ACosmicJourney = ACosmicJourney ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aCosmicJourney :: ActCard ACosmicJourney
aCosmicJourney = act (2, A) ACosmicJourney Cards.aCosmicJourney Nothing

instance HasModifiersFor ACosmicJourney where
  getModifiersFor (ACosmicJourney a) = do
    modifySelect a (EnemyWithTitle "Possessed Extra") [HealthModifier 2]

instance HasAbilities ACosmicJourney where
  getAbilities = actAbilities \a ->
    [ restricted
        a
        1
        (exists $ assetIs Assets.heliosTelescopeGateToTheCosmos <> AssetWithUseCount Shard (atLeast 3))
        $ Objective
        $ forced (RoundEnds #when)
    ]

instance RunMessage ACosmicJourney where
  runMessage msg a@(ACosmicJourney attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      discardEach attrs AnyEnemy
      spaceSet <- selectJust $ locationIs Locations.spaceSet
      eachInvestigator \iid -> do
        moveTo attrs iid spaceSet
        discardAllClues attrs iid

      selectEach (mapOneOf LocationWithTrait [Cosmos, RitualSite]) removeLocation
      selectEach (AssetWithTrait Cosmos) removeAsset

      placeSetAsideLocations_ [Locations.jungleSet, Locations.gothicSet]
      push $ SetLayout initialLayout
      lead <- getLead
      centralLot <- placeSetAsideLocation Locations.centralLotQuietOnSet
      flipOverBy lead attrs centralLot
      monarchCard <-
        selectJust $ VictoryDisplayCardMatch $ basic $ cardIs Enemies.saturniteMonarchGraciousHost
      monarch <- createEnemyAt monarchCard centralLot
      flipOverBy lead attrs monarch

      reelDeck <- take 2 <$> getScenarioDeck ReelDeck
      shuffleCardsIntoDeck Deck.EncounterDeck reelDeck
      shuffleEncounterDiscardBackIn

      addChaosToken #cultist
      addChaosToken #tablet
      addChaosToken #elderthing

      investigators <- select UneliminatedInvestigator
      heliosTelescope <- selectJust $ assetIs Assets.heliosTelescopeGateToTheCosmos
      leadChooseOneM $ targets investigators (`takeControlOfAsset` heliosTelescope)

      advanceActDeck attrs
      pure a
    _ -> ACosmicJourney <$> liftRunMessage msg attrs
