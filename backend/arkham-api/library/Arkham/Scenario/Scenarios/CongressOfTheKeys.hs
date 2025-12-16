module Arkham.Scenario.Scenarios.CongressOfTheKeys (congressOfTheKeys) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Campaigns.TheScarletKeys.Key
import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Keys
import Arkham.Campaigns.TheScarletKeys.Meta
import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Campaign
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query (allInvestigators, getLead)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher.Card
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Placement
import Arkham.Scenario.Deck
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.CongressOfTheKeys.Helpers
import Arkham.Treachery.Cards qualified as Treacheries
import Data.Map.Strict qualified as Map

newtype CongressOfTheKeys = CongressOfTheKeys ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

congressOfTheKeys :: Difficulty -> CongressOfTheKeys
congressOfTheKeys difficulty =
  scenario
    CongressOfTheKeys
    "09694"
    "Congress of the Keys"
    difficulty
    [ "coterieSanctuary1 coterieSanctuary2 coterieSanctuary3"
    , ".                 scarletHalls      ."
    ]

data Version = Version1 | Version2 | Version3
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Coterie
  = TheRedGlovedMan
  | LaChicaRoja
  | TheSanguineWatcher
  | TheBeast
  | TheClaretKnight
  | Thorne
  | Desiderio
  | Amaranth
  | TzuSanNiang
  | Aliki
  | Ece
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, FromJSONKey, ToJSONKey, ToJSON)

data Vote = Yea | Nay | Abstained | EerilySilent
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance HasChaosTokenValue CongressOfTheKeys where
  getChaosTokenValue iid tokenFace (CongressOfTheKeys attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

coterieEnemy :: Coterie -> Maybe CardDef
coterieEnemy = \case
  TheRedGlovedMan -> Just Enemies.theRedGlovedManPurposeUnknown
  LaChicaRoja -> Just Enemies.laChicaRojaHotOnYourTrail
  TheSanguineWatcher -> Just Enemies.theSanguineWatcherHeSeesWhatIsNotThere
  TheBeast -> Just Enemies.theBeastInACowlOfCrimsonLeavingATrailOfDestruction
  TheClaretKnight -> Just Enemies.theClaretKnightHoldsYouInContempt
  Thorne -> Just Enemies.thorneOpenToNegotiation
  Desiderio -> Just Enemies.desiderioDelgadoAlvarezRedInHisLedger
  Amaranth -> Just Enemies.amaranthScarletScorn
  TzuSanNiang -> Just Enemies.tzuSanNiangAWhisperInYourEar
  Aliki -> Just Enemies.alikiZoniUperetriaSpeaksInDeath
  Ece -> Nothing

conspiratorAsset :: Coterie -> Maybe CardDef
conspiratorAsset = \case
  TheRedGlovedMan -> Just Assets.theRedGlovedManHeWasAlwaysThere
  LaChicaRoja -> Just Assets.laChicaRojaYourWatchfulShadow
  TheSanguineWatcher -> Nothing
  TheBeast -> Nothing
  TheClaretKnight -> Just Assets.theClaretKnightHerSwornChampion
  Thorne -> Just Assets.thorneConsummateProfessional
  Desiderio -> Just Assets.desiderioDelgadoAlvarez
  Amaranth -> Nothing
  TzuSanNiang -> Nothing
  Aliki -> Just Assets.alikiZoniUperetriaTheMaidWithTheScarletSash
  Ece -> Just Assets.eceSahinTheVermillionVeiledLady

instance RunMessage CongressOfTheKeys where
  runMessage msg s@(CongressOfTheKeys attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      t <- getTime
      flavor do
        setTitle "title"
        p "body"
        ul do
          li.validate (t >= 35) "option1"
          li.validate (t < 35) "option2"
      flavor $ setTitle "title" >> p (if t >= 35 then "intro1" else "intro2")
      doStep 1 PreScenarioSetup
      setupKeys
      pure s
    DoStep 1 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "theTrial1"
      theCellAidedTheKnight <- getHasRecord TheCellAidedTheKnight
      theCellFailedToFendOffTheBeast <- getHasRecord TheCellFailedToFendOffTheBeast
      youHaventSeenTheLastOfTheClaretKnight <- getHasRecord YouHaventSeenTheLastOfTheClaretKnight
      theDogsAreAtWar <- getHasRecord TheDogsAreAtWar
      let
        nay1
          | theCellFailedToFendOffTheBeast = 0
          | youHaventSeenTheLastOfTheClaretKnight = 0
          | theDogsAreAtWar = 0
          | otherwise = 1
        yea1
          | theCellFailedToFendOffTheBeast = 1
          | youHaventSeenTheLastOfTheClaretKnight = 2
          | theDogsAreAtWar = 0
          | otherwise = 1

      flavor do
        cols do
          compose do
            p "nay.title"
            countVar nay1 $ p "nay.count"
          compose do
            p "yea.title"
            countVar yea1 $ p "yea.count"

        compose.red.bordered do
          p.validate theCellAidedTheKnight "theCellAidedTheKnight"
          hr
          p.validate theCellFailedToFendOffTheBeast "theCellFailedToFendOffTheBeast"
          hr
          p.validate youHaventSeenTheLastOfTheClaretKnight "youHaventSeenTheLastOfTheClaretKnight"
          hr
          p.validate theDogsAreAtWar "theDogsAreAtWar"
          hr
          p.validate
            ( not
                $ theCellAidedTheKnight
                || theCellFailedToFendOffTheBeast
                || youHaventSeenTheLastOfTheClaretKnight
                || theDogsAreAtWar
            )
            "dogsOfWarSkipped"

      unless
        ( theCellAidedTheKnight
            || theCellFailedToFendOffTheBeast
            || youHaventSeenTheLastOfTheClaretKnight
            || theDogsAreAtWar
        )
        do
          lightBearer <-
            sampleOneOf
              ( Enemies.theClaretKnightHoldsYouInContempt
              , Enemies.theBeastInACowlOfCrimsonLeavingATrailOfDestruction
              )
          setBearer Keys.theLightOfPharos (keyWithEnemy lightBearer)

      eceTrustsTheCell <- getHasRecord EceTrustsTheCell
      eceDoesNotTrustTheCell <- getHasRecord EceDoesNotTrustTheCell

      let
        nay2 =
          nay1 + case () of
            _
              | eceTrustsTheCell -> 1
              | eceDoesNotTrustTheCell -> 0
              | otherwise -> 1
        yea2 = yea1

      flavor do
        cols do
          compose do
            p "nay.title"
            countVar nay2 $ p "nay.count"
          compose do
            p "yea.title"
            countVar yea2 $ p "yea.count"

        compose.red.bordered do
          p.validate eceTrustsTheCell "eceTrustsTheCell"
          hr
          p.validate eceDoesNotTrustTheCell "eceDoesNotTrustTheCell"
          hr
          p.validate (not $ eceTrustsTheCell || eceDoesNotTrustTheCell) "dealingsInTheDarkSkipped"

      youHaventSeenTheLastOfAmaranth <- getHasRecord YouHaventSeenTheLastOfAmaranth
      theLoversAreReunited <- getHasRecord TheLoversAreReunited
      amaranthHasLeftTheCoterie <- getHasRecord TheRedCoterieWasDestroyedFromWithin

      unless (youHaventSeenTheLastOfAmaranth || theLoversAreReunited || amaranthHasLeftTheCoterie) do
        setBearer Keys.theLastBlossom (keyWithEnemy Enemies.amaranthScarletScorn)

      let
        nay3 = nay2
        yea3 =
          yea2 + case () of
            _
              | youHaventSeenTheLastOfAmaranth -> 1
              | theLoversAreReunited -> 2
              | amaranthHasLeftTheCoterie -> 0
              | otherwise -> 1

      flavor do
        cols do
          compose do
            p "nay.title"
            countVar nay3 $ p "nay.count"
          compose do
            p "yea.title"
            countVar yea3 $ p "yea.count"

        compose.red.bordered do
          p.validate youHaventSeenTheLastOfAmaranth "youHaventSeenTheLastOfAmaranth"
          hr
          p.validate theLoversAreReunited "theLoversAreReunited"
          hr
          p.validate amaranthHasLeftTheCoterie "amaranthHasLeftTheCoterie"
          hr
          p.validate
            ( not
                $ youHaventSeenTheLastOfAmaranth
                || theLoversAreReunited
                || amaranthHasLeftTheCoterie
            )
            "deadHeatSkipped"

      youHaventSeenTheLastOfThorne <- getHasRecord YouHaventSeenTheLastOfThorne
      theCellMadeADealWithThorne <- getHasRecord TheCellMadeADealWithThorne
      thorneDisappeared <- getHasRecord ThorneDisappeared

      let
        nay4 = nay3 + if theCellMadeADealWithThorne then 1 else 0
        yea4 = yea3 + if youHaventSeenTheLastOfThorne then 1 else 0

      flavor do
        cols do
          compose do
            p "nay.title"
            countVar nay4 $ p "nay.count"
          compose do
            p "yea.title"
            countVar yea4 $ p "yea.count"

        compose.red.bordered do
          p.validate youHaventSeenTheLastOfThorne "youHaventSeenTheLastOfThorne"
          hr
          p.validate theCellMadeADealWithThorne "theCellMadeADealWithThorne"
          hr
          p.validate thorneDisappeared "thorneDisappeared"
          hr
          p.validate
            ( not
                $ youHaventSeenTheLastOfThorne
                || theCellMadeADealWithThorne
                || thorneDisappeared
            )
            "onThinIceSkipped"

      unless (youHaventSeenTheLastOfThorne || theCellMadeADealWithThorne || thorneDisappeared) do
        setBearer Keys.theSableGlass (keyWithEnemy Enemies.thorneOpenToNegotiation)

      alikiIsOnYourSide <- getHasRecord AlikiIsOnYourSide
      youHaventSeenTheLastOfAlikiZoniUperetria <- getHasRecord YouHaventSeenTheLastOfAlikiZoniUperetria

      let
        nay5 = nay4 + if alikiIsOnYourSide then 1 else 0
        yea5 = yea4 + if youHaventSeenTheLastOfAlikiZoniUperetria then 1 else 0

      flavor do
        cols do
          compose do
            p "nay.title"
            countVar nay5 $ p "nay.count"
          compose do
            p "yea.title"
            countVar yea5 $ p "yea.count"

        compose.red.bordered do
          p.validate alikiIsOnYourSide "alikiIsOnYourSide"
          hr
          p.validate youHaventSeenTheLastOfAlikiZoniUperetria "youHaventSeenTheLastOfAlikiZoniUperetria"
          hr
          p.validate
            (not $ alikiIsOnYourSide || youHaventSeenTheLastOfAlikiZoniUperetria)
            "withoutATraceSkipped"

      -- desiIsInYourDebt <- getHasRecord DesiIsInYourDebt
      mdesi <- stored @CardCode "desidarioVersion"
      let desiIsGood = mdesi == Just "09607"
      let desiIsBad = mdesi == Just "09606"
      youHaventSeenTheLastOfDesiderioDelgadoAlvarez <-
        getHasRecord YouHaventSeenTheLastOfDesiderioDelgadoAlvarez
      let skippedDancingMad = not $ desiIsGood || desiIsBad || youHaventSeenTheLastOfDesiderioDelgadoAlvarez

      let
        nay6 = nay5 + if desiIsGood then 1 else 0
        yea6 = yea5 + if skippedDancingMad then 1 else 0

      flavor do
        cols do
          compose do
            p "nay.title"
            countVar nay6 $ p "nay.count"
          compose do
            p "yea.title"
            countVar yea6 $ p "yea.count"

        compose.red.bordered do
          p "desi"
          hr
          p.validate desiIsGood "desiIsGood"
          hr
          p.validate (desiIsBad || youHaventSeenTheLastOfDesiderioDelgadoAlvarez) "desiIsBad"
          hr
          p.validate skippedDancingMad "dancingMadSkipped"

      when skippedDancingMad do
        setBearer Keys.theMirroringBlade (keyWithEnemy Enemies.desiderioDelgadoAlvarezRedInHisLedger)

      theCellMeddledInAbarransAffairs <- getHasRecord TheCellMeddledInAbarransAffairs

      let
        nay7 = nay6
        yea7 = yea6 + if theCellMeddledInAbarransAffairs then 1 else 0

      flavor do
        cols do
          compose do
            p "nay.title"
            countVar nay7 $ p "nay.count"
          compose do
            p "yea.title"
            countVar yea7 $ p "yea.count"

        compose.red.bordered do
          p.validate theCellMeddledInAbarransAffairs "theCellMeddledInAbarransAffairs"
          hr
          p.validate (not theCellMeddledInAbarransAffairs) "fortuneAndFollySkipped"

      theCellKnowsTheTrueNatureOfTheCoterie <- getHasRecord TheCellKnowsTheTrueNatureOfTheCoterie
      let eerilySilentCount =
            count
              id
              [ thorneDisappeared
              , youHaventSeenTheLastOfAlikiZoniUperetria
              , youHaventSeenTheLastOfDesiderioDelgadoAlvarez || desiIsBad
              ]
      let eerilySilent = 1 + eerilySilentCount >= 3
      let continueTheTrial = not $ theCellKnowsTheTrueNatureOfTheCoterie || eerilySilent

      flavor do
        cols do
          compose do
            p "nay.title"
            countVar nay7 $ p "nay.count"
          compose do
            p "yea.title"
            countVar yea7 $ p "yea.count"

        compose.red.bordered do
          p "noMatterWhat"
          p.right.validate theCellKnowsTheTrueNatureOfTheCoterie "theCellKnowsTheTrueNatureOfTheCoterie"
          p.right.validate eerilySilent "eerilySilent"
          p.right.validate continueTheTrial "continueTheTrial"

      youHaventSeenTheLastOfLaChicaRoja <- getHasRecord YouHaventSeenTheLastOfLaChicaRoja
      youHaventSeenTheLastOfTheSanguineWatcher <- getHasRecord YouHaventSeenTheLastOfTheSanguineWatcher
      theSanguineWatchersTormentContinues <- getHasRecord TheSanguineWatchersTormentContinues

      youHaventSeenTheLastOfTzuSanNiang <- getHasRecord YouHaventSeenTheLastOfTzuSanNiang
      tzuSanNiangHasYouUnderHerSway <- getHasRecord TzuSanNiangHasYouUnderHerSway
      tzuSanNiangIsUnderYourSway <- getHasRecord TzuSanNiangIsUnderYourSway

      if
        | theCellKnowsTheTrueNatureOfTheCoterie -> doStep 6 PreScenarioSetup
        | eerilySilent -> doStep 7 PreScenarioSetup
        | otherwise -> do
            unless
              ( youHaventSeenTheLastOfLaChicaRoja
                  || youHaventSeenTheLastOfTheSanguineWatcher
                  || theSanguineWatchersTormentContinues
              )
              do
                setBearer Keys.theWeepingLady (keyWithEnemy Enemies.theSanguineWatcherHeSeesWhatIsNotThere)

            let
              laChicaRojaVotedNay = youHaventSeenTheLastOfTheSanguineWatcher || theSanguineWatchersTormentContinues

              nay8 = nay7 + if laChicaRojaVotedNay then 1 else 0
              yea8 =
                yea7 + case () of
                  _
                    | youHaventSeenTheLastOfLaChicaRoja -> 1
                    | youHaventSeenTheLastOfTheSanguineWatcher -> 1
                    | otherwise -> 0

            flavor do
              cols do
                compose do
                  p "nay.title"
                  countVar nay8 $ p "nay.count"
                compose do
                  p "yea.title"
                  countVar yea8 $ p "yea.count"

              compose.red.bordered do
                p.validate youHaventSeenTheLastOfLaChicaRoja "youHaventSeenTheLastOfLaChicaRoja"
                hr
                p.validate youHaventSeenTheLastOfTheSanguineWatcher "youHaventSeenTheLastOfTheSanguineWatcher"
                hr
                p.validate theSanguineWatchersTormentContinues "theSanguineWatchersTormentContinues"
                hr
                p.validate
                  ( not
                      $ youHaventSeenTheLastOfLaChicaRoja
                      || youHaventSeenTheLastOfTheSanguineWatcher
                      || theSanguineWatchersTormentContinues
                  )
                  "sanguineShadowsSkipped"

            unless
              (youHaventSeenTheLastOfTzuSanNiang || tzuSanNiangHasYouUnderHerSway || tzuSanNiangIsUnderYourSway)
              do
                setBearer Keys.theShadeReaper (keyWithEnemy Enemies.tzuSanNiangAWhisperInYourEar)

            let
              nay9 = nay8
              yea9 = yea8 + if tzuSanNiangIsUnderYourSway then 0 else 1

            flavor do
              cols do
                compose do
                  p "nay.title"
                  countVar nay9 $ p "nay.count"
                compose do
                  p "yea.title"
                  countVar yea9 $ p "yea.count"

              compose.red.bordered do
                p.validate youHaventSeenTheLastOfTzuSanNiang "youHaventSeenTheLastOfTzuSanNiang"
                hr
                p.validate tzuSanNiangHasYouUnderHerSway "tzuSanNiangHasYouUnderHerSway"
                hr
                p.validate tzuSanNiangIsUnderYourSway "tzuSanNiangIsUnderYourSway"
                hr
                p.validate
                  ( not
                      $ youHaventSeenTheLastOfTzuSanNiang
                      || tzuSanNiangHasYouUnderHerSway
                      || tzuSanNiangIsUnderYourSway
                  )
                  "shadesOfSufferingSkipped"

            tuwileMasaiIsOnYourSide <- getHasRecord TuwileMasaiIsOnYourSide

            let
              finalNay = nay9 + if tuwileMasaiIsOnYourSide then 1 else 0
              finalYea = yea9 + if tuwileMasaiIsOnYourSide then 0 else 1

            flavor do
              cols do
                compose do
                  p "nay.title"
                  countVar finalNay $ p "nay.count"
                compose do
                  p "yea.title"
                  countVar finalYea $ p "yea.count"

              compose.red.bordered do
                p.validate tuwileMasaiIsOnYourSide "tuwileMasaiIsOnYourSide"
                hr
                p.validate (not tuwileMasaiIsOnYourSide) "tuwileMasaiIsNotOnYourSide"

            storyWithChooseOneM' (setTitle "title" >> p "trialResult") do
              if finalYea >= finalNay
                then labeled' "deemedALiability" $ doStep 2 PreScenarioSetup
                else do
                  let canOverthrow = laChicaRojaVotedNay && eceTrustsTheCell && desiIsGood
                  let canJoin = canOverthrow && tuwileMasaiIsOnYourSide && theCellMadeADealWithThorne && theCellAidedTheKnight
                  labeledValidate' canOverthrow "overthrow" $ doStep 3 PreScenarioSetup
                  labeledValidate' canJoin "join" $ doStep 4 PreScenarioSetup
                  labeled' "deemedAnAsset" $ doStep 5 PreScenarioSetup

      let
        finishedEarly = theCellKnowsTheTrueNatureOfTheCoterie || eerilySilent
        votes =
          Map.fromList
            [ (TheRedGlovedMan, EerilySilent)
            ,
              ( LaChicaRoja
              , if
                  | finishedEarly -> Abstained
                  | youHaventSeenTheLastOfLaChicaRoja -> Abstained
                  | youHaventSeenTheLastOfTheSanguineWatcher -> Nay
                  | theSanguineWatchersTormentContinues -> Nay
                  | otherwise -> Abstained
              )
            ,
              ( TheSanguineWatcher
              , if
                  | finishedEarly -> Abstained
                  | youHaventSeenTheLastOfLaChicaRoja -> Yea
                  | youHaventSeenTheLastOfTheSanguineWatcher -> Yea
                  | theSanguineWatchersTormentContinues -> Abstained
                  | otherwise -> Abstained
              )
            ,
              ( TheBeast
              , if
                  | theCellAidedTheKnight -> Abstained
                  | theCellFailedToFendOffTheBeast -> Yea
                  | youHaventSeenTheLastOfTheClaretKnight -> Yea
                  | theDogsAreAtWar -> Abstained
                  | otherwise -> Yea
              )
            ,
              ( TheClaretKnight
              , if
                  | theCellAidedTheKnight -> Nay
                  | theCellFailedToFendOffTheBeast -> Abstained
                  | youHaventSeenTheLastOfTheClaretKnight -> Yea
                  | theDogsAreAtWar -> Abstained
                  | otherwise -> Nay
              )
            ,
              ( Thorne
              , if
                  | youHaventSeenTheLastOfThorne -> Yea
                  | theCellMadeADealWithThorne -> Nay
                  | thorneDisappeared -> EerilySilent
                  | otherwise -> Abstained
              )
            ,
              ( Desiderio
              , if
                  | desiIsGood -> Nay
                  | desiIsBad -> EerilySilent
                  | otherwise -> Yea
              )
            , (Amaranth, if amaranthHasLeftTheCoterie then Abstained else Yea)
            ,
              ( TzuSanNiang
              , if
                  | finishedEarly -> Abstained
                  | tzuSanNiangIsUnderYourSway -> Abstained
                  | otherwise -> Yea
              )
            ,
              ( Aliki
              , if
                  | alikiIsOnYourSide -> Nay
                  | youHaventSeenTheLastOfAlikiZoniUperetria -> EerilySilent
                  | otherwise -> Yea
              )
            ]

      pure $ CongressOfTheKeys $ attrs & setMetaKey "votes" votes
    DoStep 2 PreScenarioSetup -> scope "intro" do
      record TheCellEscapedTheRedCoterie
      flavor $ setTitle "title" >> p "theTrial2"
      pure $ CongressOfTheKeys $ attrs & setMetaKey "version" Version1
    DoStep 3 PreScenarioSetup -> scope "intro" do
      record TheCellOverthrewTheRedCoterie
      flavor $ setTitle "title" >> p "theTrial3"
      doStep 8 PreScenarioSetup
      pure s
    DoStep 4 PreScenarioSetup -> scope "intro" do
      record TheCellJoinedTheRedCoterie
      flavor $ setTitle "title" >> p "theTrial4"
      doStep 8 PreScenarioSetup
      pure s
    DoStep 5 PreScenarioSetup -> scope "intro" do
      record TheRedCoterieSparedTheCell
      flavor $ setTitle "title" >> p "theTrial5"
      doStep 8 PreScenarioSetup
      pure s
    DoStep 6 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "theTrial6"
      pure $ CongressOfTheKeys $ attrs & setMetaKey "version" Version3
    DoStep 7 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "theTrial7"
      pure $ CongressOfTheKeys $ attrs & setMetaKey "version" Version3
    DoStep 8 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "theTrial8"
      pure $ CongressOfTheKeys $ attrs & setMetaKey "version" Version2
    Setup -> do
      case getMetaKeyDefault "version" Version1 attrs of
        Version1 -> doStep 1 Setup
        Version2 -> doStep 2 Setup
        Version3 -> doStep 3 Setup
      pure s
    DoStep 1 Setup -> runScenarioSetup CongressOfTheKeys attrs do
      scope "version1" do
        setup' $ ul do
          li "gatherSets"
          li.nested "placeLocations" do
            li "removeSanctum"
            li "startAt"
          li "actDeck"
          li.nested "yea" do
            li "doNotGatherNay"
            li "theRedGlovedMan"
            li "drawCoterie"
            li "shuffleRemainingCoterie"
            li "yeaNote"
          li.nested "nay" do
            li "nayNote"
          li "setOutOfPlay"
          unscoped $ li "shuffleRemainder"
          li.nested "secondEncounterDeck" do
            li "otherworldDeck"
            li "shuffleRemainder"
          li "decoys"
          li "locationAdjacency"
          unscoped $ li "readyToBegin"

      gather Set.CongressOfTheKeys
      gather Set.RedCoterie
      gather Set.ScarletSorcery
      gather Set.SpatialAnomaly
      gather Set.SpreadingCorruption
      gather Set.LockedDoors

      startAt =<< place Locations.scarletHallsLair
      placeGroup
        "coterieSanctuary"
        [Locations.coterieLibraryLair, Locations.congressChamberLair, Locations.theKeyReliquaryLair]

      setActDeck [Acts.secretsAndLiesV1, Acts.toTheTower, Acts.theAscent, Acts.theFinalErr]
      setAgendaDeck [Agendas.confluxOfConsequence, Agendas.theWorldUnbidden, Agendas.runningRed]

      let votes :: Map Coterie Vote = getMetaKeyDefault "votes" mempty attrs

      lead <- getLead
      theRedGlovedMan <- fetchCard Enemies.theRedGlovedManShroudedInMystery
      drawCard lead theRedGlovedMan
      coterie <- shuffle $ mapMaybe (coterieEnemy . fst) $ filter ((== Nay) . snd) (Map.assocs votes)
      investigators <- allInvestigators
      let (toDraw, rest) = splitAt (length investigators) coterie
      for_ (zip investigators toDraw) \(iid, card) -> drawCard iid =<< fetchCard card
      addToEncounterDeck rest

      let conspirators = mapMaybe (conspiratorAsset . fst) $ filter ((== Yea) . snd) (Map.assocs votes)
      for_ conspirators \card -> do
        leadChooseOneM do
          questionLabeledCard card
          portraits investigators $ createAssetAt_ card . InPlayArea
      setAside
        [ Locations.theKnottedTower
        , Locations.gravityDefyingClimb
        , Locations.theToweringVertexRuinousConflux
        , Enemies.mimeticNemesisInfiltratorOfRealities
        ]
    DoStep 2 Setup -> runScenarioSetup CongressOfTheKeys attrs do
      scope "version2" do
        setup' $ ul do
          li.nested "gatherSets" do
            li "otherworldDeck"
          li.nested "placeLocations" do
            li "removeLair"
            li "startAt"
          li "actDeck"
          li.nested "yea" do
            li "yeaNote"
          li.nested "nay" do
            li "nayNote"
          li "setOutOfPlay"
          li "decoys"
          unscoped $ li "shuffleRemainder"
          li "locationAdjacency"
          unscoped $ li "readyToBegin"

      gather Set.CongressOfTheKeys
      gather Set.AgentsOfTheOutside
      gather Set.BeyondTheBeyond
      gather Set.Outsiders
      gather Set.RedCoterie
      gather Set.SecretWar
      gather Set.SpreadingCorruption
      gather Set.AncientEvils
      gatherJust Set.StrikingFear [Treacheries.frozenInFear, Treacheries.dissonantVoices]

      addExtraDeck OtherworldDeck =<< shuffle =<< fromGathered (CardWithTitle "City of Remnants")

      startAt =<< place Locations.scarletHallsSanctum
      placeGroup
        "coterieSanctuary"
        [ Locations.coterieLibrarySanctum
        , Locations.congressChamberSanctum
        , Locations.theKeyReliquarySanctum
        ]

      setActDeck [Acts.secretsAndLiesV2, Acts.toTheTower, Acts.theAscent, Acts.theFinalErr]
      setAgendaDeck [Agendas.confluxOfConsequence, Agendas.theWorldUnbidden, Agendas.runningRed]

      let votes :: Map Coterie Vote = getMetaKeyDefault "votes" mempty attrs
      lead <- getLead
      theRedGlovedMan <- fetchCard Enemies.theRedGlovedManShroudedInMystery
      drawCard lead theRedGlovedMan
      setAside $ mapMaybe (coterieEnemy . fst) $ filter ((== Nay) . snd) (Map.assocs votes)
      investigators <- allInvestigators

      conspirators <-
        shuffle $ mapMaybe (conspiratorAsset . fst) $ filter ((== Yea) . snd) (Map.assocs votes)
      for_ conspirators \card -> do
        leadChooseOneM do
          questionLabeledCard card
          portraits investigators $ createAssetAt_ card . InPlayArea

      setAside
        [ Locations.theKnottedTower
        , Locations.gravityDefyingClimb
        , Locations.theToweringVertexRuinousConflux
        , Enemies.mimeticNemesisInfiltratorOfRealities
        ]
    DoStep 3 Setup -> runScenarioSetup CongressOfTheKeys attrs do
      scope "version3" do
        setup' $ ul do
          li.nested "gatherSets" do
            li "otherworldDeck"
          li.nested "placeLocations" do
            li "removeLair"
            li "startAt"
          li "actDeck"
          li.nested "eerilySilent" do
            li "doNotGatherRest"
            li "theRedGlovedMan"
            li "drawCoterie"
            li "shuffleRemainingCoterie"
          li.nested "theRedCoterieSparedTheCell" do
            li "ignoreUnique"
            li "conspiratorNote"
          li "setOutOfPlay"
          li "decoys"
          unscoped $ li "shuffleRemainder"
          li "locationAdjacency"
          unscoped $ li "readyToBegin"

      gather Set.CongressOfTheKeys
      gather Set.AgentsOfTheOutside
      gather Set.BeyondTheBeyond
      gather Set.Outsiders
      gather Set.RedCoterie
      gather Set.SecretWar
      gather Set.SpreadingCorruption
      gather Set.AncientEvils
      gatherJust Set.StrikingFear [Treacheries.frozenInFear, Treacheries.dissonantVoices]

      addExtraDeck OtherworldDeck =<< shuffle =<< fromGathered (CardWithTitle "City of Remnants")

      startAt =<< place Locations.scarletHallsSanctum
      placeGroup
        "coterieSanctuary"
        [ Locations.coterieLibrarySanctum
        , Locations.congressChamberSanctum
        , Locations.theKeyReliquarySanctum
        ]

      setActDeck [Acts.secretsAndLiesV3, Acts.toTheTower, Acts.theAscent, Acts.theFinalErr]
      setAgendaDeck [Agendas.confluxOfConsequence, Agendas.theWorldUnbidden, Agendas.runningRed]

      let votes :: Map Coterie Vote = getMetaKeyDefault "votes" mempty attrs

      lead <- getLead
      theRedGlovedMan <- fetchCard Enemies.theRedGlovedManShroudedInMystery
      drawCard lead theRedGlovedMan
      coterie <-
        shuffle $ mapMaybe (coterieEnemy . fst) $ filter ((== EerilySilent) . snd) (Map.assocs votes)
      investigators <- allInvestigators
      let (toDraw, rest) = splitAt (length investigators) coterie
      for_ (zip investigators toDraw) \(iid, card) -> drawCard iid =<< fetchCard card
      addToEncounterDeck rest

      let conspirators =
            Assets.theRedGlovedManHeWasAlwaysThere
              : mapMaybe (conspiratorAsset . fst) (filter ((/= EerilySilent) . snd) (Map.assocs votes))
      for_ conspirators \card -> do
        leadChooseOneM do
          questionLabeledCard card
          portraits investigators $ createAssetAt_ card . InPlayArea

      setAside
        [ Locations.theKnottedTower
        , Locations.gravityDefyingClimb
        , Locations.theToweringVertexRuinousConflux
        , Enemies.mimeticNemesisInfiltratorOfRealities
        ]
    _ -> CongressOfTheKeys <$> liftRunMessage msg attrs
