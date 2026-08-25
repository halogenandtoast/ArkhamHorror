module Arkham.Homebrew.CircusExMortis.Scenarios.AllPointsWest (allPointsWest) where

import Arkham.Asset.Cards qualified as AssetCards
import Arkham.Calculation (GameCalculation (Fixed))
import Arkham.Card hiding (SkillType)
import Arkham.ChaosToken
import Arkham.Classes.HasGame (HasGame)
import Arkham.Helpers.Doom (getDoomCount)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.Query (getLead, getPlayerCount)
import Arkham.Homebrew.CircusExMortis.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.CircusExMortis.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.CircusExMortis.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.CircusExMortis.Helpers
import Arkham.Homebrew.CircusExMortis.Key
import Arkham.Homebrew.CircusExMortis.NowArriving
import Arkham.Homebrew.CircusExMortis.Sets qualified as Set
import Arkham.Id (InvestigatorId)
import Arkham.Investigator.Types (Field (InvestigatorHand))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Types (Field (ScenarioActStack))
import Arkham.SkillType (SkillType)
import Arkham.Token qualified as Token
import Arkham.Trait (Trait)
import Arkham.Trait qualified as Trait
import Arkham.Treachery.CardDefs.CurseOfTheRougarou qualified as Treacheries

newtype AllPointsWest = AllPointsWest ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

allPointsWest :: Difficulty -> AllPointsWest
allPointsWest difficulty =
  scenario AllPointsWest ":circus-ex-mortis:074" "All Points West" difficulty []

freightCars :: [CardDef]
freightCars =
  [ Locations.boxcar
  , Locations.flatcar
  , Locations.gondolaCar
  , Locations.stockCar
  , Locations.tankCar
  ]

specialCars :: [CardDef]
specialCars =
  [ Locations.coalHopperCar
  , Locations.craneCar
  , Locations.mailCar
  , Locations.refrigeratorCar
  , Locations.reinforcedCar
  ]

darkYoungs :: [CardDef]
darkYoungs =
  [ Enemies.loomingGoatspawn
  , Enemies.rampagingGoatspawn
  , Enemies.ravenousGoatspawn
  , Enemies.writhingGoatspawn
  ]

circusTrainLocations :: [CardDef]
circusTrainLocations =
  [Locations.circusEngine, Locations.exoticAnimalCar, Locations.performersCar]

-- | The option taken instead of the flat "place N resources" one.
data InterludeOption
  = TestOption Scope [SkillType] Int [Trait] Int
  | IconTax Scope Int [Trait]
  | AssetTax Scope Int [Trait]

data Interlude = Interlude
  { interludeKey :: Scope
  , interludeOption :: InterludeOption
  , interludeSkipLabel :: Scope
  , interludeSkipResources :: Int
  }

interludeFor :: Arrival -> Bool -> Interlude
interludeFor arrival lowDoom = case (arrival, lowDoom) of
  (ArrivingAtChicago, True) ->
    Interlude
      "notInThisTown"
      ( TestOption
          "raid"
          [#intellect, #agility]
          3
          [Trait.Police, Trait.Civic, Trait.Agency, Trait.Veteran]
          2
      )
      "smaller"
      1
  (ArrivingAtChicago, False) ->
    Interlude
      "wrongTerritory"
      (TestOption "rileUp" [#intellect, #agility] 4 [Trait.Criminal, Trait.Socialite, Trait.Entrepreneur] 3)
      "smaller"
      2
  (ArrivingAtMemphis, True) ->
    Interlude
      "stompingGround"
      ( TestOption
          "proof"
          [#willpower, #combat]
          3
          [Trait.Performer, Trait.Warden, Trait.Blessed, Trait.Cursed]
          2
      )
      "backOnTrack"
      1
  (ArrivingAtMemphis, False) ->
    Interlude
      "foolsErrand"
      (TestOption "braveDanger" [#willpower, #combat] 4 [Trait.Hunter, Trait.Believer, Trait.Chosen] 3)
      "backOnTrack"
      2
  (ArrivingAtStLouis, True) ->
    Interlude
      "natureOfTheBeast"
      (IconTax "patchThemUp" 4 [Trait.Medic, Trait.Assistant, Trait.Wayfarer, Trait.Drifter])
      "noTime"
      1
  (ArrivingAtStLouis, False) ->
    Interlude
      "skeletonsInTheCloset"
      (IconTax "whereToLook" 6 [Trait.Reporter, Trait.Detective, Trait.Clairvoyant])
      "noTime"
      2
  (ArrivingAtDenver, True) ->
    Interlude
      "behindTheCurtain"
      (AssetTax "showTheCity" 2 [Trait.Sorcerer, Trait.SilverTwilight, Trait.Scholar, Trait.Cultist])
      "cantStay"
      1
  (ArrivingAtDenver, False) ->
    Interlude
      "humaneTreatment"
      (AssetTax "paintCan" 3 [Trait.Artist, Trait.Dreamer, Trait.Miskatonic])
      "cantStay"
      2

optionLabel :: InterludeOption -> Scope
optionLabel = \case
  TestOption lbl _ _ _ _ -> lbl
  IconTax lbl _ _ -> lbl
  AssetTax lbl _ _ -> lbl

countTraits :: HasGame m => [Trait] -> m Int
countTraits traits = sum <$> traverse (selectCount . InvestigatorWithTrait) traits

-- | "Place N resources on the scenario reference card" — days the ringmaster gains.
daysBehind :: ReverseQueue m => Int -> m ()
daysBehind n = when (n > 0) $ placeTokens ScenarioSource ScenarioTarget Token.Resource n

iconTaxKey :: Text
iconTaxKey = "allPointsWest.iconTax"

assetTaxKey :: Text
assetTaxKey = "allPointsWest.assetTax"

iconCount :: Card -> Int
iconCount = length . cdSkills . toCardDef

instance HasChaosTokenValue AllPointsWest where
  getChaosTokenValue iid tokenFace (AllPointsWest attrs) = case tokenFace of
    Skull -> do
      doom <- getDoomCount
      let extra = if isHardExpert attrs then 1 else 0
      pure $ ChaosTokenValue Skull $ NegativeModifier $ extra + ((doom + 1) `div` 2)
    Cultist -> pure $ toChaosTokenValue attrs Cultist 1 2
    Tablet -> pure $ toChaosTokenValue attrs Tablet 1 2
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 2 2
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage AllPointsWest where
  runMessage msg s@(AllPointsWest attrs) = runQueueT $ scenarioI18n "allPointsWest" $ case msg of
    PreScenarioSetup -> scope "intro" do
      fromNewOrleans <- playedCurseOfTheRougarouEnRoute
      if fromNewOrleans
        then do
          scope "backOnTrack" $ flavor $ setTitle "title" >> p "body"
          -- The granted reactions themselves live on the campaign; this reads the flavor only.
          whenM (selectAny $ DeckWith $ HasCard $ cardIs Treacheries.curseOfTheRougarou)
            $ scope "whatAHorribleNight"
            $ flavor
            $ setTitle "title"
            >> p "body"
          whenM (selectAny $ DeckWith $ HasCard $ cardIs AssetCards.ladyEsprit)
            $ scope "goodJuju"
            $ flavor
            $ setTitle "title"
            >> p "body"
        else scope "rightOnSchedule" $ flavor $ setTitle "title" >> p "body"
      pure s
    Setup -> runScenarioSetup AllPointsWest attrs do
      gather Set.AllPointsWest
      gather Set.CultOfShubNiggurath
      gather Set.NewMoonDaredevils
      gather Set.PrimordialEvils

      fromNewOrleans <- playedCurseOfTheRougarouEnRoute
      let (act1, unusedAct1) =
            if fromNewOrleans
              then (Acts.throughTheForestsVII, Acts.throughTheForestsVI)
              else (Acts.throughTheForestsVI, Acts.throughTheForestsVII)
      removeEvery [unusedAct1]

      shuffledFreight <- shuffle freightCars
      shuffledSpecial <- shuffle specialCars
      let (freight, otherFreight) = splitAt 1 shuffledFreight
          (special, otherSpecial) = splitAt 1 shuffledSpecial

      locomotive <- place Locations.locomotiveEngine
      freightCar <- placeAllCapture freight
      specialCar <- placeAllCapture special
      caboose <- place Locations.caboose
      traverse_ reveal $ [locomotive, caboose] <> freightCar <> specialCar
      startAt caboose

      assetAt_ Assets.ralphDykstra locomotive
      assetAt_ Assets.carrieDykstra caboose

      setAside $ otherFreight <> otherSpecial <> darkYoungs <> circusTrainLocations

      addChaosToken $ case attrs.difficulty of
        Easy -> MinusTwo
        Standard -> MinusThree
        Hard -> MinusFour
        Expert -> MinusFive

      setAgendaDeck [Agendas.scheduleToKeep]
      setActDeck [act1, Acts.noFreeRides, Acts.engineTrouble, Acts.theGreatTrainHorror]
    ScenarioSpecific key v | key == nowArrivingKey -> do
      for_ (maybeResult v) \arrival -> do
        doom <- getDoomCount
        let interlude = interludeFor arrival (doom <= 6)
        scope "interludes" $ scope interlude.interludeKey do
          storyWithChooseOneM' (setTitle "title" >> p "body") do
            labeled' (optionLabel interlude.interludeOption) $ doStep 1 msg
            labeled' interlude.interludeSkipLabel $ daysBehind interlude.interludeSkipResources
      pure s
    DoStep 1 (ScenarioSpecific key v) | key == nowArrivingKey -> do
      for_ (maybeResult v) \arrival -> do
        doom <- getDoomCount
        case (interludeFor arrival (doom <= 6)).interludeOption of
          TestOption _ _ _ _ failResources -> do
            lead <- getLead
            setScenarioMetaKey interludeFailureKey failResources
            investigators <- select UneliminatedInvestigator
            chooseOneM lead $ targets investigators (`forInvestigator` msg)
          IconTax _ owed traits -> do
            reduction <- countTraits traits
            push $ ScenarioSpecific iconTaxKey $ toJSON $ max 0 (owed - 2 * reduction)
          AssetTax _ owed traits -> do
            reduction <- countTraits traits
            push $ ScenarioSpecific assetTaxKey $ toJSON $ max 0 (owed - reduction)
      pure s
    ForInvestigator iid (DoStep 1 (ScenarioSpecific key v)) | key == nowArrivingKey -> do
      for_ (maybeResult v) \arrival -> do
        doom <- getDoomCount
        case (interludeFor arrival (doom <= 6)).interludeOption of
          TestOption _ skills base traits _ -> do
            players <- getPlayerCount
            reduction <- countTraits traits
            let difficulty = max 0 (base + players - reduction)
            chooseOneM iid do
              for_ skills \sType -> skillLabeled sType do
                sid <- getRandom
                selectEach UneliminatedInvestigator \iid' ->
                  skillTestModifier
                    sid
                    ScenarioSource
                    iid'
                    (CanCommitToSkillTestPerformedByAnInvestigatorAt Anywhere)
                beginSkillTest sid iid ScenarioSource ScenarioTarget sType (Fixed difficulty)
          _ -> pure ()
      pure s
    ScenarioSpecific key v | key == iconTaxKey -> do
      for_ (maybeResult @Int v) \owed -> when (owed > 0) do
        lead <- getLead
        candidates <- iconTaxCandidates
        unless (null candidates) $ chooseOneM lead $ for_ candidates \(iid, card) ->
          targeting (toCardId card) do
            discardCard iid ScenarioSource card
            push $ ScenarioSpecific iconTaxKey $ toJSON $ max 0 (owed - iconCount card)
      pure s
    ScenarioSpecific key v | key == assetTaxKey -> do
      for_ (maybeResult @Int v) \owed -> when (owed > 0) do
        lead <- getLead
        assets <- select $ DiscardableAsset <> NonWeaknessAsset <> AssetControlledBy Anyone
        unless (null assets) $ chooseOneM lead $ targets assets \aid -> do
          toDiscardBy lead ScenarioSource aid
          push $ ScenarioSpecific assetTaxKey $ toJSON (owed - 1)
      pure s
    FailedSkillTest _ _ _ (isTarget ScenarioTarget -> True) _ _ -> do
      owed <- getScenarioMetaKeyDefault interludeFailureKey (0 :: Int)
      daysBehind owed
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        _ | r `elem` [NoResolution, Resolution 1] -> do
          remaining <- scenarioFieldMap ScenarioActStack (length . findWithDefault [] 1)
          resolution "resolution1"
          daysBehind (2 * remaining)
          push R5
        Resolution 2 -> do
          resolution "resolution2"
          daysBehind 2
          push R5
        Resolution 3 -> do
          resolution "resolution3"
          push R5
        Resolution 4 -> do
          resolution "resolution4"
          daysBehind 1
          push R5
        Resolution 5 -> do
          recordCountM TheRingmasterHadDaysToPrepare $ countScenarioTokens Token.Resource
          resolutionWithXp "resolution5" $ allGainXp' attrs
          endOfScenario
        _ -> error "invalid resolution"
      pure s
    _ -> AllPointsWest <$> liftRunMessage msg attrs

interludeFailureKey :: Key
interludeFailureKey = "allPointsWest.interludeFailure"

iconTaxCandidates :: HasGame m => m [(InvestigatorId, Card)]
iconTaxCandidates = do
  investigators <- select UneliminatedInvestigator
  concatMap (filter ((> 0) . iconCount . snd))
    <$> for investigators \iid -> map (iid,) <$> field InvestigatorHand iid
