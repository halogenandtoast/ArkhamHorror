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
import Arkham.Message
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.TheDevourerBelow.Story
import Arkham.Token
import Arkham.Trait hiding (Cultist)

newtype TheDevourerBelow = TheDevourerBelow ScenarioAttrs
  deriving stock (Generic)
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, NoThunks, Eq)

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
  runMessage msg s@(TheDevourerBelow attrs) = case msg of
    SetChaosTokensForScenario -> do
      whenM getIsStandalone $ push $ SetChaosTokens (chaosBagContents $ scenarioDifficulty attrs)
      pure s
    PreScenarioSetup -> do
      players <- allPlayers
      push $ story players intro
      pure s
    Setup -> do
      pastMidnight <- getHasRecord ItIsPastMidnight
      ghoulPriestAlive <- getHasRecord GhoulPriestIsStillAlive
      cultistsWhoGotAway <- getRecordSet CultistsWhoGotAway
      ghoulPriestCard <- genEncounterCard Enemies.ghoulPriest

      let
        pastMidnightMessages =
          guard pastMidnight
            *> [ AllRandomDiscard (toSource attrs) AnyCard
               , AllRandomDiscard (toSource attrs) AnyCard
               ]
        cultistsWhoGotAwayMessages =
          replicate
            ((length cultistsWhoGotAway + 1) `div` 2)
            PlaceDoomOnAgenda

      (mainPath, placeMainPath) <- placeLocationCard Locations.mainPath

      woodsLocations <-
        take 4
          <$> shuffleM
            [ Locations.arkhamWoodsUnhallowedGround
            , Locations.arkhamWoodsTwistingPaths
            , Locations.arkhamWoodsOldHouse
            , Locations.arkhamWoodsCliffside
            , Locations.arkhamWoodsTangledThicket
            , Locations.arkhamWoodsQuietGlade
            ]

      randomSet <-
        sample
          $ EncounterSet.AgentsOfYogSothoth
          :| [ EncounterSet.AgentsOfShubNiggurath
             , EncounterSet.AgentsOfCthulhu
             , EncounterSet.AgentsOfHastur
             ]

      encounterDeck <-
        buildEncounterDeckExcluding
          [Enemies.umordhoth]
          [ EncounterSet.TheDevourerBelow
          , EncounterSet.AncientEvils
          , EncounterSet.StrikingFear
          , EncounterSet.Ghouls
          , EncounterSet.DarkCult
          , randomSet
          ]

      placeWoods <- placeLabeledLocationCards_ "woods" woodsLocations

      pushAll
        $ [ SetEncounterDeck encounterDeck
          , AddChaosToken ElderThing
          , SetAgendaDeck
          , SetActDeck
          , placeMainPath
          ]
        <> placeWoods
        <> [ RevealLocation Nothing mainPath
           , MoveAllTo (toSource attrs) mainPath
           ]
        <> [AddToEncounterDeck ghoulPriestCard | ghoulPriestAlive]
        <> cultistsWhoGotAwayMessages
        <> pastMidnightMessages

      setAsideCards <- genCards [Locations.ritualSite, Enemies.umordhoth]

      acts <- genCards actDeck
      agendas <- genCards agendaDeck

      TheDevourerBelow
        <$> runMessage
          msg
          ( attrs
              & (setAsideCardsL .~ setAsideCards)
              & (actStackL . at 1 ?~ acts)
              & (agendaStackL . at 1 ?~ agendas)
          )
    ResolveChaosToken _ Cultist iid -> do
      let doom = if isEasyStandard attrs then 1 else 2
      closestEnemyIds <- selectList $ NearestEnemy AnyEnemy
      player <- getPlayer iid
      case closestEnemyIds of
        [] -> pure ()
        [x] -> push $ PlaceTokens (ChaosTokenEffectSource Cultist) (toTarget x) Doom doom
        xs ->
          push
            $ chooseOne player
            $ [targetLabel x [PlaceTokens (ChaosTokenEffectSource Cultist) (toTarget x) Doom doom] | x <- xs]
      pure s
    ResolveChaosToken _ Tablet iid -> do
      let horror = if isEasyStandard attrs then 0 else 1
      pushWhenM (selectAny $ EnemyAt (locationWithInvestigator iid) <> EnemyWithTrait Monster)
        $ assignDamageAndHorror iid (ChaosTokenEffectSource Tablet) 1 horror
      pure s
    ResolveChaosToken _ ElderThing iid -> do
      pushWhenM (selectAny $ EnemyWithTrait AncientOne) $ DrawAnotherChaosToken iid
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget (chaosTokenFace -> Skull)) _ _ | isHardExpert attrs -> do
      push $ findAndDrawEncounterCard iid $ CardWithType EnemyType <> CardWithTrait Monster
      pure s
    ScenarioResolution r -> do
      let
        (resolution, record) = case r of
          NoResolution ->
            (noResolution, ArkhamSuccumbedToUmordhothsTerribleVengeance)
          Resolution 1 -> (resolution1, TheRitualToSummonUmordhothWasBroken)
          Resolution 2 -> (resolution2, TheInvestigatorsRepelledUmordoth)
          Resolution 3 ->
            (resolution3, TheInvestigatorsSacrificedLitaChantlerToUmordhoth)
          _ -> error "Invalid resolution"
      players <- allPlayers
      pushAll [story players resolution, Record record, EndOfGame Nothing]
      pure s
    HandleOption option -> do
      whenM getIsStandalone $ do
        lead <- getLeadPlayer
        investigators <- allInvestigators
        case option of
          AddLitaChantler -> push $ forceAddCampaignCardToDeckChoice lead investigators Assets.litaChantler
          _ -> error $ "Unhandled option: " <> show option
      pure s
    _ -> TheDevourerBelow <$> runMessage msg attrs
