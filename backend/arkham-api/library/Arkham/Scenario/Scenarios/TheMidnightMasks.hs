module Arkham.Scenario.Scenarios.TheMidnightMasks (setupTheMidnightMasks, theMidnightMasks, TheMidnightMasks (..)) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaign.Option
import Arkham.Campaigns.NightOfTheZealot.Import
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types
import Arkham.Helpers.Agenda (getCurrentAgendaStep)
import Arkham.Helpers.Doom
import Arkham.Helpers.EncounterSet
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (enemyAt)
import Arkham.Message.Lifted hiding (setActDeck, setAgendaDeck)
import Arkham.Message.Lifted.Log
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.Scenario.Import.Lifted hiding (
  chooseOrRunOne,
  createEnemyAt,
  placeLocationCard,
  story,
 )
import Arkham.Scenarios.TheMidnightMasks.Helpers
import Arkham.Token
import Arkham.Trait qualified as Trait
import Control.Lens (non)

newtype TheMidnightMasks = TheMidnightMasks ScenarioAttrs
  deriving stock Generic
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

theMidnightMasks :: Difficulty -> TheMidnightMasks
theMidnightMasks difficulty =
  scenarioWith
    TheMidnightMasks
    "01120"
    "The Midnight Masks"
    difficulty
    [ "northside downtown easttown"
    , "miskatonicUniversity rivertown graveyard"
    , "stMarysHospital southside yourHouse"
    ]
    (decksL .~ mapFromList [(CultistDeck, [])])

instance HasChaosTokenValue TheMidnightMasks where
  getChaosTokenValue iid chaosTokenFace (TheMidnightMasks attrs) = case chaosTokenFace of
    Skull -> do
      value <- byDifficulty attrs (fieldMax EnemyDoom (EnemyWithTrait Trait.Cultist)) getDoomCount
      pure $ ChaosTokenValue Skull (NegativeModifier value)
    Cultist -> pure $ ChaosTokenValue Cultist (NegativeModifier 2)
    Tablet -> pure $ toChaosTokenValue attrs Tablet 3 4
    otherFace -> getChaosTokenValue iid otherFace attrs

setupTheMidnightMasks :: (HasI18n, ReverseQueue m) => ScenarioAttrs -> ScenarioBuilderT m ()
setupTheMidnightMasks _attrs = do
  burnedToTheGround <- getHasRecord YourHouseHasBurnedToTheGround
  ghoulPriestStillAlive <- getHasRecord GhoulPriestIsStillAlive
  n <- getPlayerCount
  setup $ ul do
    li "gatherSets"
    li "cultistDeck"
    li "placeLocations"

    li.nested "acolytes.instructions" do
      li.validate (n == 1) "acolytes.onePlayer"
      li.validate (n == 2) "acolytes.twoPlayer"
      li.validate (n == 3) "acolytes.threePlayer"
      li.validate (n == 4) "acolytes.fourPlayer"

    li.validate burnedToTheGround "burnedToTheGround"
    li.validate (not burnedToTheGround) "houseStillStanding"

    unscoped $ li "shuffleRemainder"

    li.validate ghoulPriestStillAlive "ghoulPriest"

  whenReturnTo $ gather Set.ReturnToTheMidnightMasks
  gather Set.TheMidnightMasks
  gather Set.ChillingCold
  gather Set.Nightgaunts
  gather Set.LockedDoors
  gather Set.DarkCult `orWhenReturnTo` gather Set.TheDevourersCult

  isReturnTo <- getIsReturnTo
  predatorOrPrey <-
    if isReturnTo
      then sample2 Agendas.predatorOrPrey Agendas.returnToPredatorOrPrey
      else pure Agendas.predatorOrPrey
  setAgendaDeck [predatorOrPrey, Agendas.timeIsRunningShort]
  setActDeck [Acts.uncoveringTheConspiracy]

  rivertown <-
    if isReturnTo
      then placeOneOf (Locations.rivertown, Locations.rivertownAbandonedWarehouse)
      else place Locations.rivertown

  southside <- placeOneOf (Locations.southsideHistoricalSociety, Locations.southsideMasBoardingHouse)
  downtown <- placeOneOf (Locations.downtownFirstBankOfArkham, Locations.downtownArkhamAsylum)
  graveyard <- place Locations.graveyard

  if isReturnTo
    then do
      place_ Locations.stMarysHospital
      placeOneOf_ (Locations.miskatonicUniversity, Locations.miskatonicUniversityMiskatonicMuseum)
      placeOneOf_ (Locations.easttown, Locations.easttownArkhamPoliceStation)
      placeOneOf_ (Locations.northside, Locations.northsideTrainStation)
    else
      placeAll
        [Locations.easttown, Locations.miskatonicUniversity, Locations.northside, Locations.stMarysHospital]

  cultOfUmordhoth <- gatherEncounterSet Set.CultOfUmordhoth
  cultistCards <-
    if isReturnTo
      then do
        returnCultOfUmordhoth <- gatherEncounterSet Set.ReturnCultOfUmordhoth
        drop 3 <$> shuffle (cultOfUmordhoth <> returnCultOfUmordhoth)
      else pure cultOfUmordhoth
  addExtraDeck CultistDeck =<< shuffle cultistCards

  if burnedToTheGround
    then startAt rivertown
    else startAt =<< place Locations.yourHouse

  count' <- getPlayerCount
  let acolytes = replicate (count' - 1) Enemies.acolyte
  zipWithM_ enemyAt acolytes [southside, downtown, graveyard]

  when ghoulPriestStillAlive $ addToEncounterDeck (only Enemies.ghoulPriest)

instance RunMessage TheMidnightMasks where
  runMessage msg s@(TheMidnightMasks attrs) = runQueueT $ scenarioI18n $ case msg of
    StandaloneSetup -> do
      setChaosTokens (chaosBagContents attrs.difficulty)
      pure s
    PreScenarioSetup -> do
      flavor $ scope "intro" do
        h "title"
        p "body"
      forcedToFindOthers <- getHasRecord LitaWasForcedToFindOthersToHelpHerCause
      doStep (if forcedToFindOthers then 1 else 2) msg
      pure s
    DoStep 1 PreScenarioSetup -> do
      story $ i18n "intro1"
      doStep 3 PreScenarioSetup
      pure s
    DoStep 2 PreScenarioSetup -> do
      story $ i18n "intro2"
      doStep 3 PreScenarioSetup
      pure s
    DoStep 3 PreScenarioSetup -> do
      story $ i18n "intro3"
      pure s
    Setup -> runScenarioSetup TheMidnightMasks attrs $ setupTheMidnightMasks attrs
    ResolveChaosToken _ Cultist iid | isEasyStandard attrs -> do
      closestCultists <- select $ NearestEnemyToFallback iid $ EnemyWithTrait Trait.Cultist
      when (notNull closestCultists) do
        chooseOrRunOne iid
          $ [ targetLabel x [PlaceTokens (ChaosTokenEffectSource Cultist) (toTarget x) Doom 1]
            | x <- closestCultists
            ]
      pure s
    ResolveChaosToken _ Cultist iid | isHardExpert attrs -> do
      cultists <- select $ EnemyWithTrait Trait.Cultist
      case cultists of
        [] -> push $ DrawAnotherChaosToken iid
        xs -> pushAll [PlaceTokens (ChaosTokenEffectSource Cultist) (toTarget eid) Doom 1 | eid <- xs]
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget (chaosTokenFace -> Tablet)) _ _ -> do
      push
        $ byDifficulty
          attrs
          (InvestigatorPlaceCluesOnLocation iid (ChaosTokenEffectSource Tablet) 1)
          (InvestigatorPlaceAllCluesOnLocation iid (ChaosTokenEffectSource Tablet))
      pure s
    ScenarioResolution NoResolution -> scope "resolutions" do
      resolution "noResolution"
      push R1
      pure s
    ScenarioResolution (Resolution n) -> scope "resolutions" do
      agenda <- getCurrentAgendaStep
      inPlayCultistsWhoGotAway <-
        selectField EnemyCardCode (InPlayEnemy $ withTrait Trait.Cultist <> UniqueEnemy)
      ghoulPriestDefeated <- selectAny (VictoryDisplayCardMatch $ basic $ cardIs Enemies.ghoulPriest)
      resolutionWithXp
        (if n == 1 then "resolution1" else "resolution2")
        (allGainXp' attrs)
      recordSetInsert CultistsWeInterrogated
        =<< selectMap toCardCode (VictoryDisplayCardMatch $ basic $ withTrait Trait.Cultist <> CardIsUnique)
      recordSetInsert CultistsWhoGotAway
        $ inPlayCultistsWhoGotAway
        <> map toCardCode (attrs ^. decksL . at CultistDeck . non [])
        <> [toCardCode Enemies.theMaskedHunter | agenda == 1]
      recordWhen (n == 2) ItIsPastMidnight
      crossOutWhen ghoulPriestDefeated GhoulPriestIsStillAlive
      endOfScenario
      pure s
    HandleOption option -> do
      whenM getIsStandalone $ case option of
        AddLitaChantler -> do
          investigators <- allInvestigators
          forceAddCampaignCardToDeckChoice investigators ShuffleIn Assets.litaChantler
        _ -> error $ "Unhandled option: " <> show option
      pure s
    _ -> TheMidnightMasks <$> liftRunMessage msg attrs
