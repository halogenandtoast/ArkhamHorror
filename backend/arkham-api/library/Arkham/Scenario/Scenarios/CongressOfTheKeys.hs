module Arkham.Scenario.Scenarios.CongressOfTheKeys (congressOfTheKeys) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheScarletKeys.Concealed (
  ConcealedCardKind (CityOfRemnantsL, CityOfRemnantsM, CityOfRemnantsR),
  mkConcealedCard,
 )
import Arkham.Campaigns.TheScarletKeys.Concealed.Types
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Campaigns.TheScarletKeys.Helpers qualified as Meta
import Arkham.Campaigns.TheScarletKeys.Key
import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Keys
import Arkham.Campaigns.TheScarletKeys.Key.Matcher
import Arkham.Campaigns.TheScarletKeys.Meta
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.ForMovement
import Arkham.Helpers (Deck (..))
import Arkham.Helpers.Act
import Arkham.Helpers.Campaign
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Location (getCanMoveTo, withLocationOf)
import Arkham.Helpers.Query (allInvestigators, getLead)
import Arkham.Helpers.SkillTest (isFightWith, withSkillTest)
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Modifier
import Arkham.Placement
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.CongressOfTheKeys.Helpers
import Arkham.Trait (Trait (Outsider))
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Window qualified as Window
import Control.Lens (non, _1)
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
    Skull -> do
      n <- getCurrentActStep
      pure $ toChaosTokenValue attrs Skull n (n + 1)
    Cultist -> pure $ toChaosTokenValue attrs Cultist 5 7
    Tablet -> pure $ ChaosTokenValue Tablet (NegativeModifier 2)
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 3 4
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

coterieEnemies, conspiratorAssets :: ScenarioAttrs -> (Vote -> Bool) -> [CardDef]
coterieEnemies attrs f = overVotes attrs f coterieEnemy
conspiratorAssets attrs f = overVotes attrs f conspiratorAsset

overVotes :: ScenarioAttrs -> (Vote -> Bool) -> (Coterie -> Maybe a) -> [a]
overVotes attrs f g =
  Map.assocs votes & mapMaybe \(coterie, vote) -> guard (f vote) *> g coterie
 where
  votes = getMetaKeyDefault "votes" mempty attrs

toSetAside :: [CardDef]
toSetAside =
  [ Locations.theKnottedTower
  , Locations.gravityDefyingClimb
  , Locations.theToweringVertexRuinousConflux
  , Enemies.mimeticNemesisInfiltratorOfRealities
  ]

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
      eceTrustsTheCell <- getHasRecord EceTrustsTheCell
      eceDoesNotTrustTheCell <- getHasRecord EceDoesNotTrustTheCell
      youHaventSeenTheLastOfAmaranth <- getHasRecord YouHaventSeenTheLastOfAmaranth
      theLoversAreReunited <- getHasRecord TheLoversAreReunited
      amaranthHasLeftTheCoterie <- getHasRecord TheRedCoterieWasDestroyedFromWithin
      youHaventSeenTheLastOfThorne <- getHasRecord YouHaventSeenTheLastOfThorne
      theCellMadeADealWithThorne <- getHasRecord TheCellMadeADealWithThorne
      thorneDisappeared <- getHasRecord ThorneDisappeared
      alikiIsOnYourSide <- getHasRecord AlikiIsOnYourSide
      youHaventSeenTheLastOfAlikiZoniUperetria <- getHasRecord YouHaventSeenTheLastOfAlikiZoniUperetria
      theCellMeddledInAbarransAffairs <- getHasRecord TheCellMeddledInAbarransAffairs
      theCellKnowsTheTrueNatureOfTheCoterie <- getHasRecord TheCellKnowsTheTrueNatureOfTheCoterie
      youHaventSeenTheLastOfLaChicaRoja <- getHasRecord YouHaventSeenTheLastOfLaChicaRoja
      youHaventSeenTheLastOfTheSanguineWatcher <- getHasRecord YouHaventSeenTheLastOfTheSanguineWatcher
      theSanguineWatchersTormentContinues <- getHasRecord TheSanguineWatchersTormentContinues
      youHaventSeenTheLastOfTzuSanNiang <- getHasRecord YouHaventSeenTheLastOfTzuSanNiang
      tzuSanNiangHasYouUnderHerSway <- getHasRecord TzuSanNiangHasYouUnderHerSway
      tzuSanNiangIsUnderYourSway <- getHasRecord TzuSanNiangIsUnderYourSway
      tuwileMasaiIsOnYourSide <- getHasRecord TuwileMasaiIsOnYourSide
      youHaventSeenTheLastOfDesiderioDelgadoAlvarez <-
        getHasRecord YouHaventSeenTheLastOfDesiderioDelgadoAlvarez

      -- desiIsInYourDebt <- getHasRecord DesiIsInYourDebt
      mdesi <- stored @CardCode "desidarioVersion"
      let desiIsGood = mdesi == Just "09607"
      let desiIsBad = mdesi == Just "09606"
      let showVotes nay yea =
            cols do
              compose $ countVar nay $ p "nay.title" >> p "nay.count"
              compose $ countVar yea $ p "yea.title" >> p "yea.count"

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

      let dogsOfWarSkipped =
            not
              $ theCellAidedTheKnight
              || theCellFailedToFendOffTheBeast
              || youHaventSeenTheLastOfTheClaretKnight
              || theDogsAreAtWar
      flavor do
        showVotes nay1 yea1
        compose.red.bordered do
          p.validate theCellAidedTheKnight "theCellAidedTheKnight"
          hr
          p.validate theCellFailedToFendOffTheBeast "theCellFailedToFendOffTheBeast"
          hr
          p.validate youHaventSeenTheLastOfTheClaretKnight "youHaventSeenTheLastOfTheClaretKnight"
          hr
          p.validate theDogsAreAtWar "theDogsAreAtWar"
          hr
          p.validate dogsOfWarSkipped "dogsOfWarSkipped"

      when dogsOfWarSkipped do
        sampleOneOf
          ( Enemies.theClaretKnightHoldsYouInContempt
          , Enemies.theBeastInACowlOfCrimsonLeavingATrailOfDestruction
          )
          >>= (setBearer Keys.theLightOfPharos . keyWithEnemy)

      let
        nay2 = nay1 + if eceDoesNotTrustTheCell then 0 else 1
        yea2 = yea1

      flavor do
        showVotes nay2 yea2
        compose.red.bordered do
          p.validate eceTrustsTheCell "eceTrustsTheCell"
          hr
          p.validate eceDoesNotTrustTheCell "eceDoesNotTrustTheCell"
          hr
          p.validate (not $ eceTrustsTheCell || eceDoesNotTrustTheCell) "dealingsInTheDarkSkipped"

      unless (youHaventSeenTheLastOfAmaranth || theLoversAreReunited || amaranthHasLeftTheCoterie) do
        setBearer Keys.theLastBlossom (keyWithEnemy Enemies.amaranthScarletScorn)

      let
        nay3 = nay2
        yea3 =
          yea2
            + if
              | youHaventSeenTheLastOfAmaranth -> 1
              | theLoversAreReunited -> 2
              | amaranthHasLeftTheCoterie -> 0
              | otherwise -> 1

      flavor do
        showVotes nay3 yea3
        compose.red.bordered do
          p.validate youHaventSeenTheLastOfAmaranth "youHaventSeenTheLastOfAmaranth"
          hr
          p.validate theLoversAreReunited "theLoversAreReunited"
          hr
          p.validate amaranthHasLeftTheCoterie "amaranthHasLeftTheCoterie"
          hr
          p.validate
            (not $ youHaventSeenTheLastOfAmaranth || theLoversAreReunited || amaranthHasLeftTheCoterie)
            "deadHeatSkipped"

      let
        nay4 = nay3 + if theCellMadeADealWithThorne then 1 else 0
        yea4 = yea3 + if youHaventSeenTheLastOfThorne then 1 else 0

      flavor do
        showVotes nay4 yea4
        compose.red.bordered do
          p.validate youHaventSeenTheLastOfThorne "youHaventSeenTheLastOfThorne"
          hr
          p.validate theCellMadeADealWithThorne "theCellMadeADealWithThorne"
          hr
          p.validate thorneDisappeared "thorneDisappeared"
          hr
          p.validate
            (not $ youHaventSeenTheLastOfThorne || theCellMadeADealWithThorne || thorneDisappeared)
            "onThinIceSkipped"

      unless (youHaventSeenTheLastOfThorne || theCellMadeADealWithThorne || thorneDisappeared) do
        setBearer Keys.theSableGlass (keyWithEnemy Enemies.thorneOpenToNegotiation)

      let
        nay5 = nay4 + if alikiIsOnYourSide then 1 else 0
        yea5 = yea4 + if youHaventSeenTheLastOfAlikiZoniUperetria then 1 else 0

      flavor do
        showVotes nay5 yea5
        compose.red.bordered do
          p.validate alikiIsOnYourSide "alikiIsOnYourSide"
          hr
          p.validate youHaventSeenTheLastOfAlikiZoniUperetria "youHaventSeenTheLastOfAlikiZoniUperetria"
          hr
          p.validate
            (not $ alikiIsOnYourSide || youHaventSeenTheLastOfAlikiZoniUperetria)
            "withoutATraceSkipped"

      let skippedDancingMad = not $ desiIsGood || desiIsBad || youHaventSeenTheLastOfDesiderioDelgadoAlvarez

      let
        nay6 = nay5 + if desiIsGood then 1 else 0
        yea6 = yea5 + if skippedDancingMad then 1 else 0

      flavor do
        showVotes nay6 yea6
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

      let
        nay7 = nay6
        yea7 = yea6 + if theCellMeddledInAbarransAffairs then 1 else 0

      flavor do
        showVotes nay7 yea7
        compose.red.bordered do
          p.validate theCellMeddledInAbarransAffairs "theCellMeddledInAbarransAffairs"
          hr
          p.validate (not theCellMeddledInAbarransAffairs) "fortuneAndFollySkipped"

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
        showVotes nay7 yea7
        compose.red.bordered do
          p "noMatterWhat"
          p.right.validate theCellKnowsTheTrueNatureOfTheCoterie "theCellKnowsTheTrueNatureOfTheCoterie"
          p.right.validate eerilySilent "eerilySilent"
          p.right.validate continueTheTrial "continueTheTrial"

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
                yea7
                  + if
                    | youHaventSeenTheLastOfLaChicaRoja -> 1
                    | youHaventSeenTheLastOfTheSanguineWatcher -> 1
                    | otherwise -> 0

            flavor do
              showVotes nay8 yea8
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
              showVotes nay9 yea9
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

            let
              finalNay = nay9 + if tuwileMasaiIsOnYourSide then 1 else 0
              finalYea = yea9 + if tuwileMasaiIsOnYourSide then 0 else 1

            flavor do
              showVotes finalNay finalYea
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

      setUsesGrid
      gather Set.CongressOfTheKeys
      gather Set.ScarletSorcery
      gather Set.SpatialAnomaly
      gather Set.SpreadingCorruption
      gather Set.LockedDoors

      -- extra deck, it is easier to gather it now since we can use the
      -- scenario helpers
      gather Set.AgentsOfTheOutside
      gather Set.BeyondTheBeyond
      gather Set.Outsiders
      gather Set.SecretWar
      gather Set.AncientEvils
      gatherJust Set.StrikingFear [Treacheries.frozenInFear, Treacheries.dissonantVoices]

      setExtraEncounterDeck SetAsideEncounterDeck
        =<< amongGathered
          ( SingleSidedCard
              <> mapOneOf
                CardFromEncounterSet
                [ Set.AgentsOfTheOutside
                , Set.BeyondTheBeyond
                , Set.Outsiders
                , Set.SecretWar
                , Set.AncientEvils
                , Set.StrikingFear
                ]
              <> not_ #location
          )
      addExtraDeck OtherworldDeck =<< shuffle =<< fromGathered (CardWithTitle "City of Remnants")

      startAt =<< placeInGrid (Pos 0 0) Locations.scarletHallsLair
      coterieSanctuaries <-
        shuffle [Locations.coterieLibraryLair, Locations.congressChamberLair, Locations.theKeyReliquaryLair]
      zipWithM_ placeInGrid_ [Pos (-1) 1, Pos 0 1, Pos 1 1] coterieSanctuaries

      setActDeck [Acts.secretsAndLiesV1, Acts.toTheTower, Acts.theAscent, Acts.theFinalErr]
      setAgendaDeck [Agendas.confluxOfConsequence, Agendas.theWorldUnbidden, Agendas.runningRed]

      lead <- getLead
      coterie <- shuffle $ coterieEnemies attrs (== Nay)
      investigators <- allInvestigators
      drawCard lead Enemies.theRedGlovedManPurposeUnknown
      let (toDraw, rest) = splitAt (length investigators) coterie
      zipWithM_ drawCard investigators toDraw
      addToEncounterDeck rest

      for_ (conspiratorAssets attrs (== Yea)) \card -> do
        leadChooseOneM do
          questionLabeledCard card
          portraits investigators $ createAssetAt_ card . InPlayArea
      setAside toSetAside
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

      setUsesGrid
      gather Set.CongressOfTheKeys
      gather Set.AgentsOfTheOutside
      gather Set.BeyondTheBeyond
      gather Set.Outsiders
      gather Set.SecretWar
      gather Set.SpreadingCorruption
      gather Set.AncientEvils
      gatherJust Set.StrikingFear [Treacheries.frozenInFear, Treacheries.dissonantVoices]

      addExtraDeck OtherworldDeck =<< shuffle =<< fromGathered (CardWithTitle "City of Remnants")

      startAt =<< placeInGrid (Pos 0 0) Locations.scarletHallsSanctum
      coterieSanctuaries <-
        shuffle
          [ Locations.coterieLibrarySanctum
          , Locations.congressChamberSanctum
          , Locations.theKeyReliquarySanctum
          ]
      zipWithM_ placeInGrid_ [Pos (-1) 1, Pos 0 1, Pos 1 1] coterieSanctuaries

      setActDeck [Acts.secretsAndLiesV2, Acts.toTheTower, Acts.theAscent, Acts.theFinalErr]
      setAgendaDeck [Agendas.confluxOfConsequence, Agendas.theWorldUnbidden, Agendas.runningRed]

      lead <- getLead
      drawCard lead Enemies.theRedGlovedManPurposeUnknown
      setAside $ coterieEnemies attrs (== Nay)

      investigators <- allInvestigators
      for_ (conspiratorAssets attrs (== Yea)) \card -> do
        leadChooseOneM do
          questionLabeledCard card
          portraits investigators $ createAssetAt_ card . InPlayArea

      setAside toSetAside
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

      setUsesGrid
      gather Set.CongressOfTheKeys
      gather Set.AgentsOfTheOutside
      gather Set.BeyondTheBeyond
      gather Set.Outsiders
      gather Set.SecretWar
      gather Set.SpreadingCorruption
      gather Set.AncientEvils
      gatherJust Set.StrikingFear [Treacheries.frozenInFear, Treacheries.dissonantVoices]

      addExtraDeck OtherworldDeck =<< shuffle =<< fromGathered (CardWithTitle "City of Remnants")

      startAt =<< placeInGrid (Pos 0 0) Locations.scarletHallsSanctum
      coterieSanctuaries <-
        shuffle
          [ Locations.coterieLibrarySanctum
          , Locations.congressChamberSanctum
          , Locations.theKeyReliquarySanctum
          ]
      zipWithM_ placeInGrid_ [Pos (-1) 1, Pos 0 1, Pos 1 1] coterieSanctuaries

      setActDeck [Acts.secretsAndLiesV3, Acts.toTheTower, Acts.theAscent, Acts.theFinalErr]
      setAgendaDeck [Agendas.confluxOfConsequence, Agendas.theWorldUnbidden, Agendas.runningRed]

      lead <- getLead
      coterie <-
        shuffle $ filter (/= Enemies.theRedGlovedManPurposeUnknown) $ coterieEnemies attrs (== EerilySilent)
      investigators <- allInvestigators
      drawCard lead Enemies.theRedGlovedManPurposeUnknown
      let (toDraw, rest) = splitAt (length investigators) coterie
      zipWithM_ drawCard investigators toDraw
      addToEncounterDeck rest

      for_ (Assets.theRedGlovedManHeWasAlwaysThere : conspiratorAssets attrs (/= EerilySilent)) \card -> do
        leadChooseOneM do
          questionLabeledCard card
          portraits investigators $ createAssetAt_ card . InPlayArea

      setAside toSetAside
    ResolveChaosToken drawnToken Cultist iid -> do
      controlledKeys <- select $ StableScarletKey <> ScarletKeyWithBearer (InvestigatorWithId iid)
      unless (null controlledKeys) do
        chooseOneM iid do
          when (isEasyStandard attrs) $ labeled' "cultist.easyStandard" do
            chooseTargetM iid controlledKeys (flipOverBy iid Cultist)
            chaosTokenEffect ElderThing drawnToken $ ChaosTokenFaceModifier [MinusTwo]
          when (isHardExpert attrs) $ labeled' "cultist.hardExpert" do
            chooseTargetM iid controlledKeys (flipOverBy iid Cultist)
            chaosTokenEffect ElderThing drawnToken $ ChaosTokenFaceModifier [MinusFour]
          unscoped skip_
      pure s
    ResolveChaosToken _ Tablet iid -> do
      ok <-
        if isEasyStandard attrs
          then isFightWith $ EnemyWithTrait Outsider <> ReadyEnemy
          else selectAny $ EnemyAt (locationWithInvestigator iid) <> EnemyWithTrait Outsider <> ReadyEnemy
      when ok failSkillTest
      pure s
    ResolveChaosToken drawnToken ElderThing _iid -> do
      withSkillTest \sid -> do
        skillTestModifier sid drawnToken.face sid CancelSkills
        push CancelSkillEffects
        cards <- concat <$> selectField InvestigatorCommittedCards Anyone
        for_ cards \c -> for_ c.owner (`hollow` c)
      pure s
    ScenarioSpecific "shuffleAllConcealed" _ -> do
      concealedCards <-
        select ConcealedCardAny >>= mapMaybeM \c -> runMaybeT do
          InPosition pos <- lift $ field ConcealedCardPlacement c.id
          pure (c, pos)
      let (cards, positions) = unzip concealedCards
      shuffled <- shuffle cards
      lead <- getLead
      for_ (zip shuffled positions) \(card, pos) -> do
        push $ PlaceConcealedCard lead (toId card) (InPosition pos)
      pure s
    ScenarioSpecific "setupOtherworld" v -> do
      let otherworldDeck = attrs ^. decksL . at OtherworldDeck . non []
      let (inPlay, rest) = splitAt 3 otherworldDeck

      meta <- case inPlay of
        [x, y, z] -> do
          ll <- placeLocation x
          ml <- placeLocation y
          rl <- placeLocation z
          for_ [ll, ml, rl] \location -> do
            push $ UpdateLocation location (Update LocationPlacement (Just InTheShadows))
          pure
            $ LocationsInShadowsMetadata
              { locationsInShadows = LocationsInShadows (Just ll) (Just ml) (Just rl)
              , concealedCards = mempty
              }
        _ -> error "expected exactly three locations in play"

      cards <-
        shuffle =<< traverse mkConcealedCard [CityOfRemnantsL, CityOfRemnantsM, CityOfRemnantsR]

      lead <- getLead
      for_ (zip cards [Pos 1 0, Pos 0 (-1), Pos (-1) 0]) \(card, pos) -> do
        push $ Msg.CreateConcealedCard card
        push $ Msg.PlaceConcealedCard lead (toId card) (InPosition pos)

      let
        setDeck =
          case toResult v of
            Version1 -> const (attrs ^. encounterDecksL . at SetAsideEncounterDeck . non (Deck [], []) . _1)
            _ -> id
      pure
        $ CongressOfTheKeys
        $ attrs
        & (encounterDecksL .~ mempty)
        & (encounterDeckL %~ setDeck)
        & (decksL . at OtherworldDeck ?~ rest)
        & (metaL .~ toJSON meta)
    LookAtTopOfDeck iid (ScenarioDeckTarget OtherworldDeck) n -> do
      case fromJustNote "must be set" (lookup OtherworldDeck attrs.decks) of
        cards -> focusCards (map flipCard $ take n cards) $ continue_ iid
      pure s
    ScenarioSpecific "swapLocations" v -> do
      let (l1, l2) :: (LocationId, LocationId) = toResult v
      let meta = toResult @LocationsInShadowsMetadata attrs.meta
      let
        swapLocation loc =
          if
            | loc == l1 -> l2
            | loc == l2 -> l1
            | otherwise -> loc
      let left = swapLocation <$> meta.locationsInShadows.left
      let middle = swapLocation <$> meta.locationsInShadows.middle
      let right = swapLocation <$> meta.locationsInShadows.right
      let locationsInShadows = LocationsInShadows left middle right
      pure
        $ CongressOfTheKeys
        $ attrs
        & (metaL .~ toJSON (meta {locationsInShadows}))
    ScenarioSpecific "swapMiniCards" v -> do
      let (c1, c2) :: (ConcealedCardId, ConcealedCardId) = toResult v
      let meta = toResult @LocationsInShadowsMetadata attrs.meta
      let
        swapMiniCard c =
          if
            | c == c1 -> c2
            | c == c2 -> c1
            | otherwise -> c
      let concealedCards = Map.map (map swapMiniCard) meta.concealedCards
      pure
        $ CongressOfTheKeys
        $ attrs
        & (metaL .~ toJSON (meta {concealedCards}))
    ScenarioSpecific "distributeConcealedLocations" v -> do
      let (iid, cs, current, original) :: (InvestigatorId, [ConcealedCard], [Pos], [Pos]) = toResult v
      case cs of
        [] -> pure ()
        _ | length cs >= length current -> do
          for_ (zip cs current) \(x, pos) -> do
            push $ PlaceConcealedCard iid (toId x) (InPosition pos)
          scenarioSpecific "distributeConcealedLocations" (iid, drop (length current) cs, original, original)
        (x : xs) ->
          if null current
            then scenarioSpecific "distributeConcealedLocations" (iid, cs, original, original)
            else chooseOneM iid do
              for_ (eachWithRest current) \(pos, rest) -> do
                gridLabeled (gridLabel pos) do
                  push $ PlaceConcealedCard iid (toId x) (InPosition pos)
                  scenarioSpecific "distributeConcealedLocations" (iid, xs, rest, original)
      pure s
    PlaceConcealedCard _ card (InPosition pos) -> do
      let meta = toResult @LocationsInShadowsMetadata attrs.meta
      let current = Map.findWithDefault [] pos meta.concealedCards
      cards <- shuffleM $ nub $ card : current
      let concealedCards = Map.map (filter (/= card)) meta.concealedCards
      pure
        $ CongressOfTheKeys
        $ attrs
        & metaL
        .~ toJSON (meta {concealedCards = Map.insert pos cards concealedCards})
    Do (PlaceConcealedCard _ card (InPosition pos)) -> do
      let meta = toResult @LocationsInShadowsMetadata attrs.meta
      let current = Map.findWithDefault [] pos meta.concealedCards
      cards <- shuffleM $ nub $ card : current
      let concealedCards = Map.map (filter (/= card)) meta.concealedCards
      pure
        $ CongressOfTheKeys
        $ attrs
        & metaL
        .~ toJSON (meta {concealedCards = Map.insert pos cards concealedCards})
    Do (ScenarioSpecific "exposed[CityOfRemnantsL]" v) -> do
      CongressOfTheKeys
        <$> handleCityOfRemnants attrs v (.left) (\locations ml -> locations {Meta.left = ml})
    Do (ScenarioSpecific "exposed[CityOfRemnantsM]" v) -> do
      CongressOfTheKeys
        <$> handleCityOfRemnants attrs v (.middle) (\locations ml -> locations {middle = ml})
    Do (ScenarioSpecific "exposed[CityOfRemnantsR]" v) -> do
      CongressOfTheKeys
        <$> handleCityOfRemnants attrs v (.right) (\locations ml -> locations {right = ml})
    ScenarioSpecific "exposed[CityOfRemnantsL]" v -> do
      let (iid, _c) :: (InvestigatorId, ConcealedCard) = toResult v
      let meta = toResult @LocationsInShadowsMetadata attrs.meta
      let locationsInShadows = meta.locationsInShadows
      for_ locationsInShadows.left \loc -> do
        scenarioSpecific "exposed[CityOfRemnants]" (iid, LeftPosition, loc)
        do_ msg
        forTarget_ loc msg
      pure s
    ScenarioSpecific "exposed[CityOfRemnantsM]" v -> do
      let (iid, _c) :: (InvestigatorId, ConcealedCard) = toResult v
      let meta = toResult @LocationsInShadowsMetadata attrs.meta
      let locationsInShadows = meta.locationsInShadows
      for_ locationsInShadows.middle \loc -> do
        scenarioSpecific "exposed[CityOfRemnants]" (iid, MiddlePosition, loc)
        do_ msg
        forTarget_ loc msg
      pure s
    ScenarioSpecific "exposed[CityOfRemnantsR]" v -> do
      let (iid, _c) :: (InvestigatorId, ConcealedCard) = toResult v
      let meta = toResult @LocationsInShadowsMetadata attrs.meta
      let locationsInShadows = meta.locationsInShadows
      for_ locationsInShadows.right \loc -> do
        scenarioSpecific "exposed[CityOfRemnants]" (iid, RightPosition, loc)
        do_ msg
        forTarget_ loc msg
      pure s
    ForTarget (LocationTarget loc) (ScenarioSpecific x v)
      | x `elem` ["exposed[CityOfRemnantsL]", "exposed[CityOfRemnantsM]", "exposed[CityOfRemnantsR]"] -> do
          let (iid, _c) :: (InvestigatorId, ConcealedCard) = toResult v
          withLocationOf iid \current -> do
            whenMatch loc (ConnectedTo ForMovement $ LocationWithId current) do
              whenM (getCanMoveTo iid attrs loc) do
                checkAfter $ Window.ScenarioEvent "exposedAdjacentLocation" (Just iid) (toJSON loc)

          checkAfter $ Window.CampaignEvent "exposed[location]" (Just iid) Null
          pure s
    RemoveLocation lid -> do
      attrs' <- liftRunMessage msg attrs
      case maybeResult @LocationsInShadowsMetadata attrs.meta of
        Nothing -> pure $ CongressOfTheKeys attrs'
        Just meta -> do
          let
            guardLocation a = guard (a /= Just lid) *> a
            meta' =
              meta
                { locationsInShadows =
                    LocationsInShadows
                      { left = guardLocation meta.locationsInShadows.left
                      , middle = guardLocation meta.locationsInShadows.middle
                      , right = guardLocation meta.locationsInShadows.right
                      }
                }
          pure $ CongressOfTheKeys $ attrs' & metaL .~ toJSON meta'
    _ -> CongressOfTheKeys <$> liftRunMessage msg attrs
