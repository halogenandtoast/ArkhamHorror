module Arkham.Scenario.Scenarios.AllOrNothing (allOrNothing, AllOrNothing (..)) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Event.Cards qualified as Events
import Arkham.Exception
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Xp
import Arkham.Investigator.Types (Field (InvestigatorClues, InvestigatorResources))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.AllOrNothing.Helpers
import Arkham.Treachery.Cards qualified as Treacheries

{- FOURMOLU_DISABLE -}
easyTokens, standardTokens, hardTokens, expertTokens :: [ChaosTokenFace]
easyTokens =
  [ PlusOne , PlusOne , Zero , Zero , Zero , MinusOne , MinusOne , MinusOne , MinusTwo , MinusTwo
  , Skull , Skull , Cultist , Tablet , ElderThing , AutoFail , ElderSign
  ]
standardTokens =
  [ PlusOne , Zero , Zero , MinusOne , MinusOne , MinusOne , MinusTwo , MinusTwo , MinusThree
  , MinusFour , Skull , Skull , Cultist , Tablet , ElderThing , AutoFail , ElderSign
  ]
hardTokens =
  [ Zero , Zero , MinusOne , MinusOne , MinusTwo , MinusTwo , MinusThree , MinusFour , MinusFive
  , MinusSix , Skull , Skull , Cultist , Tablet , ElderThing , AutoFail , ElderSign
  ]
expertTokens =
  [ Zero , MinusOne , MinusTwo , MinusThree , MinusFour , MinusFive , MinusSix , MinusSeven
  , MinusEight , Skull , Skull , Cultist , Tablet , ElderThing , AutoFail , ElderSign
  ]
{- FOURMOLU_ENABLE -}

newtype AllOrNothing = AllOrNothing ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

allOrNothing :: Difficulty -> AllOrNothing
allOrNothing difficulty =
  sideStory
    AllOrNothing
    "90011"
    "All or Nothing"
    difficulty
    [ ".    .      .        backHallDoorway1 ."
    , ".    .      triangle backHallDoorway1 ."
    , "moon circle triangle diamond          backHallDoorway2"
    , "moon circle square   diamond          backHallDoorway2"
    , ".    .      square   backHallDoorway3 ."
    , ".    .      .        backHallDoorway3 ."
    ]

instance HasChaosTokenValue AllOrNothing where
  getChaosTokenValue iid chaosTokenFace (AllOrNothing attrs) = case chaosTokenFace of
    Skull -> do
      clues <- field InvestigatorClues iid
      pure $ ChaosTokenValue Skull $ NegativeModifier $ if isEasyStandard attrs then clues else 2 * clues
    Cultist -> do
      resources <- field InvestigatorResources iid
      let base = if isEasyStandard attrs then 2 else 4
      pure $ ChaosTokenValue Cultist $ NegativeModifier $ if resources >= 10 then base * 2 else base
    Tablet -> pure $ toChaosTokenValue attrs Tablet 2 3
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 3 5
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage AllOrNothing where
  runMessage msg s@(AllOrNothing attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      flavor $ scope "intro" do
        h "title"
        p "body"
      pure s
    StandaloneSetup -> do
      setChaosTokens $ case attrs.difficulty of
        Easy -> easyTokens
        Standard -> standardTokens
        Hard -> hardTokens
        Expert -> expertTokens
      pure s
    Setup -> runScenarioSetup AllOrNothing attrs do
      setup do
        ul do
          li "gatherSets"
          li "newActAgenda"
          li "removeFromGame"
          li "setAside"
          li "placeLocations"
          li "placePitBoss"
          unscoped $ li "shuffleRemainder"
          unscoped $ li "readyToBegin"

      gather Set.AllOrNothing
      gather Set.TheHouseAlwaysWins
      gather Set.BadLuck
      gather Set.NaomisCrew
      gather Set.Rats
      gatherJust Set.TheMidnightMasks [Treacheries.falseLead, Treacheries.huntingShadow]

      removeEvery [Assets.peterClover, Assets.drFrancisMorgan]

      startAt =<< place Locations.laBellaLuna
      cloverClubLounge <- place Locations.cloverClubLounge
      placeAll [Locations.cloverClubBar, Locations.cloverClubCardroom, Locations.darkenedHall]
      enemyAt_ Enemies.cloverClubPitBoss cloverClubLounge

      setAside
        $ [ Locations.artGallery
          , Locations.vipArea
          , Locations.backAlley
          , Enemies.siobhanRiley
          ]
        <> replicate 4 Enemies.cloverClubBouncer

      setActDeck [Acts.playingCards, Acts.hotOnYourTail]
      setAgendaDeck [Agendas.eyesAllAroundYou]
    ResolveChaosToken _ Tablet iid | isHardExpert attrs -> do
      loseResources iid Tablet 3
      pure s
    FailedSkillTestWithToken iid Tablet | isEasyStandard attrs -> do
      loseResources iid Tablet 3
      pure s
    FailedSkillTestWithToken _ ElderThing -> do
      selectOne skidsOToole >>= traverse_ \skids -> assignHorror skids ElderThing 1
      pure s
    ScenarioSpecific "placeResourcesOnAct" v -> do
      let total = getMetaKeyDefault "actResources" 0 attrs
      pure $ AllOrNothing $ setMetaKey "actResources" (total + toResultDefault 0 v :: Int) attrs
    ScenarioResolution r -> scope "resolutions" do
      mSkids <- selectOne $ IncludeEliminated skidsOToole
      case r of
        NoResolution -> do
          resolution "noResolution"
          push R2
        Resolution 1 -> do
          let resources = getMetaKeyDefault "actResources" 0 attrs :: Int
          let (bonusXp, bonusResources)
                | resources >= 60 = (3, 6)
                | resources >= 50 = (2, 5)
                | resources >= 40 = (1, 4)
                | resources >= 30 = (0, 3)
                | resources >= 20 = (0, 2)
                | resources >= 10 = (0, 1)
                | otherwise = (0, 0)
          withVars
            [ "resources" .= resources
            , "bonusXp" .= bonusXp
            , "bonusResources" .= bonusResources
            ]
            $ resolutionWithXp "resolution1"
            $ allGainXp' attrs
          for_ mSkids \skids -> do
            when (bonusXp > 0) $ interludeXp skids $ toBonus "gamblersTake" bonusXp
            when (bonusResources > 0) $ recordCount AllOrNothingBonusResources bonusResources
            hasOnTheLam <- hasCardInDeck skids Events.onTheLam
            hasAdvancedHospitalDebts <- hasCardInDeck skids Treacheries.hospitalDebtsAdvanced
            chooseOneM skids do
              questionLabeled' "skidsMaySwap"
              when hasOnTheLam $ labeled' "upgradeOnTheLam" do
                removeCampaignCardFromDeck skids Events.onTheLam
                addCampaignCardToDeck skids DoNotShuffleIn Events.onTheLamAdvanced
              when hasAdvancedHospitalDebts $ labeled' "downgradeHospitalDebts" do
                removeCampaignCardFromDeck skids Treacheries.hospitalDebtsAdvanced
                addCampaignCardToDeck skids DoNotShuffleIn Treacheries.hospitalDebts
              labeled' "doNotSwap" nothing
          endOfScenario
        Resolution 2 -> do
          resolutionWithXp "resolution2" $ allGainXp' attrs
          for_ mSkids \skids -> do
            hasHospitalDebts <- hasCardInDeck skids Treacheries.hospitalDebts
            hasAdvancedOnTheLam <- hasCardInDeck skids Events.onTheLamAdvanced
            when (hasHospitalDebts || hasAdvancedOnTheLam) do
              chooseOrRunOneM skids do
                questionLabeled' "skidsMustSwap"
                when hasHospitalDebts $ labeled' "upgradeHospitalDebts" do
                  removeCampaignCardFromDeck skids Treacheries.hospitalDebts
                  addCampaignCardToDeck skids DoNotShuffleIn Treacheries.hospitalDebtsAdvanced
                when hasAdvancedOnTheLam $ labeled' "downgradeOnTheLam" do
                  removeCampaignCardFromDeck skids Events.onTheLamAdvanced
                  addCampaignCardToDeck skids DoNotShuffleIn Events.onTheLam
          endOfScenario
        other -> throwIO $ UnknownResolution other
      pure s
    _ -> AllOrNothing <$> liftRunMessage msg attrs
