module Arkham.Scenario.Scenarios.ByTheBook (byTheBook, ByTheBook (..)) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.ChaosBag (getBagChaosTokens)
import Arkham.Helpers.EncounterSet
import Arkham.Helpers.FlavorText
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.ByTheBook.Helpers
import Arkham.Trait qualified as Trait
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

newtype ByTheBook = ByTheBook ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

byTheBook :: Difficulty -> ByTheBook
byTheBook difficulty =
  sideStory
    ByTheBook
    "90032"
    "By the Book"
    difficulty
    [ "northside downtown easttown"
    , "miskatonicUniversity rivertown graveyard"
    , "stMarysHospital southside arkhamPoliceStation"
    ]

instance HasChaosTokenValue ByTheBook where
  getChaosTokenValue iid tokenFace (ByTheBook attrs) = case tokenFace of
    Skull -> do
      n <- selectCount victoryDisplayCultists
      pure $ ChaosTokenValue Skull $ NegativeModifier $ byDifficulty attrs (min 5 n) (1 + n)
    Cultist -> pure $ toChaosTokenValue attrs Cultist 2 3
    Tablet -> pure $ toChaosTokenValue attrs Tablet 3 4
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 4 5
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage ByTheBook where
  runMessage msg s@(ByTheBook attrs) = runQueueT $ scenarioI18n $ case msg of
    StandaloneSetup -> do
      setChaosTokens $ case attrs.difficulty of
        Easy -> easyTokens
        Standard -> standardTokens
        Hard -> hardTokens
        Expert -> expertTokens
      pure s
    PreScenarioSetup -> do
      flavor $ scope "intro" $ h "title" >> p "body"
      pure s
    Setup -> runScenarioSetup ByTheBook attrs do
      setup $ ul do
        li "gatherSets"
        li "removeCards"
        li "chooseLocations"
        li.nested "placeLocations" do
          li "startAt"
        li "conspirators"
        li "rolandClue"
        unscoped $ li "shuffleRemainder"
        unscoped $ li "readyToBegin"

      gather Set.TheMidnightMasks
      gather Set.AgentsOfShubNiggurath
      gather Set.ChillingCold
      gather Set.Nightgaunts
      gather Set.StrikingFear

      setAgendaDeck [Agendas.aCovertConspiracy, Agendas.yourDeadlineNears]
      setActDeck [Acts.captureTheConspirators]

      downtown <- placeOneOf (Locations.downtownFirstBankOfArkham, Locations.downtownArkhamAsylum)
      southside <-
        placeOneOf (Locations.southsideHistoricalSociety, Locations.southsideMasBoardingHouse)
      others <-
        placeAllCapture
          [ Locations.northside
          , Locations.easttown
          , Locations.rivertown
          , Locations.stMarysHospital
          , Locations.graveyard
          , Locations.miskatonicUniversity
          ]
      arkhamPoliceStation <- place Locations.arkhamPoliceStationByTheBook
      startAt arkhamPoliceStation

      cultOfUmordhoth <- gatherEncounterSet Set.CultOfUmordhoth
      darkCult <- gatherEncounterSet Set.DarkCult
      let darkCultEnemies = filterCards (CardWithType EnemyType) (map toCard darkCult)
      conspirators <-
        traverse (setFacedown True) =<< shuffle (map toCard cultOfUmordhoth <> darkCultEnemies)
      zipWithM_
        (\lid conspirator -> placeUnderneath lid [conspirator])
        (downtown : southside : arkhamPoliceStation : others)
        conspirators

      selectOne rolandBanks >>= traverse_ \roland -> gainClues roland ScenarioSource 1
    ResolveChaosToken _ Cultist iid -> do
      whenM (selectAny $ EnemyWithTrait Trait.Cultist <> enemyEngagedWith iid) do
        drawAnotherChaosToken iid
      pure s
    ResolveChaosToken _ Tablet iid -> do
      selectEach (EnemyWithTrait Trait.Cultist <> enemyEngagedWith iid <> ExhaustedEnemy) ready
      pure s
    FailedSkillTest _ _ _ (ChaosTokenTarget (chaosTokenFace -> ElderThing)) _ _ -> do
      selectOne rolandBanks >>= traverse_ \roland ->
        assignDamage roland (ChaosTokenEffectSource ElderThing) 1
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> do
          rolandResigned <- selectAny $ IncludeEliminated $ ResignedInvestigator <> rolandBanks
          push $ if rolandResigned then R1 else R2
        Resolution 1 -> do
          (rolandXp, otherXp) <- gainByTheBookXp (toSource attrs)
          withVars ["rolandXp" .= rolandXp, "otherXp" .= otherXp] $ resolution "resolution1"
          mRoland <- selectOne $ IncludeEliminated rolandBanks
          n <- selectCount victoryDisplayCultists
          let bonus
                | n >= 8 = 3
                | n >= 6 = 2
                | n >= 4 = 1
                | otherwise = 0
          when (bonus > 0) $ recordCount ByTheBookBonusCards bonus
          when (n >= 10) do
            faces <- sort . nub . filter isNumberChaosToken . map (.face) <$> getBagChaosTokens
            for_ mRoland \roland ->
              chooseOneM roland do
                questionLabeled' "removeChaosToken"
                for_ faces \face -> chaosTokenLabeled face $ push $ RemoveChaosToken face
          mrGreyCaptured <- selectAny $ VictoryDisplayCardMatch $ basic $ cardIs Enemies.mrGrey
          when mrGreyCaptured $ for_ mRoland \roland -> do
            hasRolands38Special <- hasDeckCard roland Assets.rolands38Special
            hasAdvancedCoverUp <- hasDeckCard roland Treacheries.coverUpAdvanced
            when (hasRolands38Special || hasAdvancedCoverUp) do
              chooseOneM roland do
                questionLabeled' "advancedReward"
                labeled' "doNotSwap" nothing
                when hasRolands38Special do
                  labeled' "upgradeRolands38Special"
                    $ swapCampaignCard roland Assets.rolands38Special Assets.rolands38SpecialAdvanced
                when hasAdvancedCoverUp do
                  labeled' "downgradeCoverUp"
                    $ swapCampaignCard roland Treacheries.coverUpAdvanced Treacheries.coverUp
          endOfScenario
        Resolution 2 -> do
          (rolandXp, otherXp) <- gainByTheBookXp (toSource attrs)
          withVars ["rolandXp" .= rolandXp, "otherXp" .= otherXp] $ resolution "resolution2"
          mRoland <- selectOne $ IncludeEliminated rolandBanks
          for_ mRoland \roland -> do
            hasCoverUp <- hasDeckCard roland Treacheries.coverUp
            hasAdvancedRolands38Special <- hasDeckCard roland Assets.rolands38SpecialAdvanced
            case (hasCoverUp, hasAdvancedRolands38Special) of
              (True, True) -> chooseOneM roland do
                questionLabeled' "advancedPenalty"
                labeled' "upgradeCoverUp"
                  $ swapCampaignCard roland Treacheries.coverUp Treacheries.coverUpAdvanced
                labeled' "downgradeRolands38Special"
                  $ swapCampaignCard roland Assets.rolands38SpecialAdvanced Assets.rolands38Special
              (True, False) -> swapCampaignCard roland Treacheries.coverUp Treacheries.coverUpAdvanced
              (False, True) ->
                swapCampaignCard roland Assets.rolands38SpecialAdvanced Assets.rolands38Special
              (False, False) -> pure ()
          endOfScenario
        _ -> error $ "Unknown resolution for ByTheBook: " <> show r
      pure s
    _ -> ByTheBook <$> liftRunMessage msg attrs
