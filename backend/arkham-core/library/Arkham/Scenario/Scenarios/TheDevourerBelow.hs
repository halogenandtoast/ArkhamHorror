module Arkham.Scenario.Scenarios.TheDevourerBelow where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaign.Option
import Arkham.CampaignLogKey
import Arkham.Campaigns.NightOfTheZealot.ChaosBag
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Message hiding (chooseOrRunOne, story)
import Arkham.Message.Lifted hiding (setActDeck, setAgendaDeck)
import Arkham.Resolution
import Arkham.Scenario.Helpers hiding (forceAddCampaignCardToDeckChoice)
import Arkham.Scenario.Runner hiding (
  assignDamageAndHorror,
  chooseOrRunOne,
  findAndDrawEncounterCard,
  placeLocationCard,
  story,
 )
import Arkham.Scenario.Setup
import Arkham.Scenarios.TheDevourerBelow.Story
import Arkham.Token
import Arkham.Trait hiding (Cultist)

newtype TheDevourerBelow = TheDevourerBelow ScenarioAttrs
  deriving stock (Generic)
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

theDevourerBelow :: Difficulty -> TheDevourerBelow
theDevourerBelow difficulty =
  scenario
    TheDevourerBelow
    "01142"
    "The Devourer Below"
    difficulty
    [ "woods1     .     woods2"
    , "woods1 mainPath woods2"
    , "woods3 mainPath woods4"
    , "woods3 ritualSite woods4"
    , "   .   ritualSite   .  "
    ]

instance HasChaosTokenValue TheDevourerBelow where
  getChaosTokenValue iid chaosTokenFace (TheDevourerBelow attrs) = case chaosTokenFace of
    Skull -> do
      monsterCount <- selectCount $ EnemyWithTrait Monster
      pure $ toChaosTokenValue attrs Skull monsterCount 3
    Cultist -> pure $ toChaosTokenValue attrs Cultist 2 4
    Tablet -> pure $ toChaosTokenValue attrs Tablet 3 5
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 5 7
    otherFace -> getChaosTokenValue iid otherFace attrs

actDeck :: [CardDef]
actDeck =
  [Acts.investigatingTheTrail, Acts.intoTheDarkness, Acts.disruptingTheRitual]

agendaDeck :: [CardDef]
agendaDeck =
  [Agendas.theArkhamWoods, Agendas.theRitualBegins, Agendas.vengeanceAwaits]

instance RunMessage TheDevourerBelow where
  runMessage msg s@(TheDevourerBelow attrs) = runQueueT $ case msg of
    SetChaosTokensForScenario -> do
      whenM getIsStandalone $ push $ SetChaosTokens (chaosBagContents $ scenarioDifficulty attrs)
      pure s
    PreScenarioSetup -> do
      story intro
      pure s
    Setup -> runScenarioSetup TheDevourerBelow attrs do
      gather EncounterSet.TheDevourerBelow
      gather EncounterSet.AncientEvils
      gather EncounterSet.StrikingFear
      gather EncounterSet.Ghouls
      gather EncounterSet.DarkCult
      gatherOneOf
        $ EncounterSet.AgentsOfYogSothoth
        :| [EncounterSet.AgentsOfShubNiggurath, EncounterSet.AgentsOfCthulhu, EncounterSet.AgentsOfHastur]

      setAside [Locations.ritualSite, Enemies.umordhoth]
      whenHasRecord GhoulPriestIsStillAlive $ addToEncounterDeck (Only Enemies.ghoulPriest)

      setActDeck actDeck
      setAgendaDeck agendaDeck
      addChaosToken ElderThing

      startAt =<< place Locations.mainPath
      placeGroupChooseN 4 "woods"
        $ Locations.arkhamWoodsUnhallowedGround
        :| [ Locations.arkhamWoodsTwistingPaths
           , Locations.arkhamWoodsOldHouse
           , Locations.arkhamWoodsCliffside
           , Locations.arkhamWoodsTangledThicket
           , Locations.arkhamWoodsQuietGlade
           ]

      cultistsWhoGotAway <- getRecordSet CultistsWhoGotAway
      let placeDoomAmount = (length cultistsWhoGotAway + 1) `div` 2
      pushWhen (placeDoomAmount > 0) $ PlaceDoomOnAgenda placeDoomAmount CanNotAdvance

      whenHasRecord ItIsPastMidnight
        $ pushAll
          [ AllRandomDiscard (toSource attrs) AnyCard
          , AllRandomDiscard (toSource attrs) AnyCard
          ]
    ResolveChaosToken _ Cultist iid -> do
      let doom = if isEasyStandard attrs then 1 else 2
      closestEnemyIds <- select $ NearestEnemy AnyEnemy
      when (notNull closestEnemyIds) do
        chooseOrRunOne iid
          $ [ targetLabel x [PlaceTokens (ChaosTokenEffectSource Cultist) (toTarget x) Doom doom]
            | x <- closestEnemyIds
            ]
      pure s
    ResolveChaosToken _ Tablet iid -> do
      let horror = byDifficulty attrs 0 1
      whenM (selectAny $ EnemyAt (locationWithInvestigator iid) <> EnemyWithTrait Monster)
        $ assignDamageAndHorror iid (ChaosTokenEffectSource Tablet) 1 horror
      pure s
    ResolveChaosToken _ ElderThing iid -> do
      pushWhenM (selectAny $ EnemyWithTrait AncientOne) $ DrawAnotherChaosToken iid
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget (chaosTokenFace -> Skull)) _ _ | isHardExpert attrs -> do
      findAndDrawEncounterCard iid $ CardWithType EnemyType <> CardWithTrait Monster
      pure s
    ScenarioResolution r -> do
      let
        (resolution, key) = case r of
          NoResolution -> (noResolution, ArkhamSuccumbedToUmordhothsTerribleVengeance)
          Resolution 1 -> (resolution1, TheRitualToSummonUmordhothWasBroken)
          Resolution 2 -> (resolution2, TheInvestigatorsRepelledUmordoth)
          Resolution 3 -> (resolution3, TheInvestigatorsSacrificedLitaChantlerToUmordhoth)
          _ -> error "Invalid resolution"
      story resolution
      record key
      endOfScenario
      pure s
    HandleOption option -> do
      whenM getIsStandalone $ do
        case option of
          AddLitaChantler -> do
            investigators <- allInvestigators
            forceAddCampaignCardToDeckChoice investigators Assets.litaChantler
          _ -> error $ "Unhandled option: " <> show option
      pure s
    _ -> TheDevourerBelow <$> liftRunMessage msg attrs
