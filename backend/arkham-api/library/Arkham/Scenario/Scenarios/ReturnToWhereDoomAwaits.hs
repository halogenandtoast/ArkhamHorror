module Arkham.Scenario.Scenarios.ReturnToWhereDoomAwaits (returnToWhereDoomAwaits) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheDunwichLegacy.Key
import Arkham.Deck qualified as Deck
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.EncounterSet
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Log
import Arkham.Helpers.Query (allInvestigators)
import Arkham.Location.Cards qualified as Locations
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.WhereDoomAwaits
import Arkham.Scenarios.WhereDoomAwaits.Helpers

newtype ReturnToWhereDoomAwaits = ReturnToWhereDoomAwaits WhereDoomAwaits
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

returnToWhereDoomAwaits :: Difficulty -> ReturnToWhereDoomAwaits
returnToWhereDoomAwaits difficulty =
  scenarioWith
    (ReturnToWhereDoomAwaits . WhereDoomAwaits)
    "51047"
    "Return to Where Doom Awaits"
    difficulty
    [ "divergingPath1 divergingPath2 divergingPath3"
    , "baseOfTheHill ascendingPath sentinelPeak"
    , "alteredPath1 alteredPath2 alteredPath3"
    ]
    (referenceL .~ "02274")

instance RunMessage ReturnToWhereDoomAwaits where
  runMessage msg (ReturnToWhereDoomAwaits whereDoomAwaits'@(WhereDoomAwaits attrs)) = runQueueT $ scenarioI18n $ case msg of
    Setup -> runScenarioSetup (ReturnToWhereDoomAwaits . WhereDoomAwaits) attrs do
      useV1 <- getHasRecord TheInvestigatorsRestoredSilasBishop
      useV2 <-
        liftA2
          (||)
          (getHasRecord TheInvestigatorsFailedToRecoverTheNecronomicon)
          (getHasRecord TheNecronomiconWasStolen)

      setup do
        ul do
          li "gatherSets"
          li "placeLocations"
          li "divergingPaths"
          li "alteredPaths"
          li "setAside"
          li "adjustChaosBag"
          li.nested "act2.instructions" do
            li.validate useV1 "act2.v1"
            li.validate useV2 "act2.v2"
            li.validate (not $ useV1 || useV2) "act2.v3"
          li "addDoom"
          li "hideousAbominations"
          unscoped $ li "shuffleRemainder"

      gather Set.ReturnToWhereDoomAwaits
      gather Set.WhereDoomAwaits
      gather Set.BeastThralls
      gather Set.Sorcery
      gather Set.BishopsThralls
      gather Set.ErraticFear
      gather Set.ResurgentEvils
      gather Set.CreepingCold

      noBroodEscaped <- getHasRecord NoBroodEscapedIntoTheWild
      broodEscapedCount <- if noBroodEscaped then pure 0 else getRecordCount BroodEscapedIntoTheWild
      silasBishopPutOutOfMisery <- getHasRecord TheInvestigatorsPutSilasBishopOutOfHisMisery

      startAt =<< place Locations.baseOfTheHillWarpedAndTwisted
      ascendingPath <- place Locations.ascendingPathWarpedAndTwisted
      place_ Locations.sentinelPeak

      when silasBishopPutOutOfMisery do
        (conglomerationOfSpheres, rest) <- splitAt 1 <$> gatherEncounterSet Set.HideousAbominations
        for_ conglomerationOfSpheres (`createEnemyAt_` ascendingPath)
        shuffleCardsIntoDeck Deck.EncounterDeck rest

      divergingPaths <-
        sampleN 3
          $ Locations.slaughteredWoods
          :| [ Locations.eerieGlade
             , Locations.destroyedPath
             , Locations.frozenSpring
             , Locations.abandonedCamp
             ]

      alteredPaths <-
        sampleN 3
          $ Locations.dimensionalGap
          :| [ Locations.aTearInThePath
             , Locations.uprootedWoods
             , Locations.lostMemories
             , Locations.fathomlessLake
             ]

      addChaosToken $ case attrs.difficulty of
        Easy -> MinusThree
        Standard -> MinusFive
        Hard -> MinusSix
        Expert -> MinusSeven

      when (broodEscapedCount > 0) $ placeDoomOnAgenda broodEscapedCount

      setAside $ Assets.naomiOBannionRuthlessTactician
        : Enemies.sethBishop
        : divergingPaths <> alteredPaths
      setAgendaDeck [Agendas.callingForthTheOldOnes, Agendas.beckoningForPower]

      whenHasRecord NaomiHasTheInvestigatorsBacks do
        investigators <- allInvestigators
        addCampaignCardToDeckChoice investigators ShuffleIn Assets.naomiOBannionRuthlessTactician

      let
        ascendingTheHill = case (useV1, useV2) of
          (True, _) -> Acts.ascendingTheHillV1
          (False, True) -> Acts.ascendingTheHillV2
          (False, False) -> Acts.ascendingTheHillV3
      setActDeck [Acts.thePathToTheHill, ascendingTheHill, Acts.theGateOpens]
    _ -> ReturnToWhereDoomAwaits <$> liftRunMessage msg whereDoomAwaits'
