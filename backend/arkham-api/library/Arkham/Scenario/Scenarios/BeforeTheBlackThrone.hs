module Arkham.Scenario.Scenarios.BeforeTheBlackThrone (setupBeforeTheBlackThrone, beforeTheBlackThrone, BeforeTheBlackThrone (..)) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheCircleUndone.Key
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario hiding (getIsReturnTo)
import Arkham.Helpers.Scenario qualified as Scenario
import Arkham.Helpers.SkillTest
import Arkham.Helpers.Xp
import Arkham.I18n
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Placement
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.BeforeTheBlackThrone.Cosmos
import Arkham.Scenarios.BeforeTheBlackThrone.Helpers
import Arkham.Token qualified as Token
import Arkham.Trait qualified as Trait
import Data.Aeson (Result (..))

-- * Cosmos

{- $cosmos
The cosmos is an internal data structure that represents a grid which can
grow in any direction. Additionally, we need to track if there is empty
space or a card at a specific position. Finally, cards can slide around.
Because this logic needs to be known by locations we store in the
`scenarioMeta` field. That way it can be passed around and set accordingly.
-}

newtype BeforeTheBlackThrone = BeforeTheBlackThrone ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beforeTheBlackThrone :: Difficulty -> BeforeTheBlackThrone
beforeTheBlackThrone difficulty = scenario BeforeTheBlackThrone "05325" "Before the Black Throne" difficulty []

instance HasChaosTokenValue BeforeTheBlackThrone where
  getChaosTokenValue iid tokenFace (BeforeTheBlackThrone attrs) = case tokenFace of
    Skull -> do
      x <- selectJustField EnemyDoom (IncludeOmnipotent $ enemyIs Enemies.azathoth)
      pure $ toChaosTokenValue attrs Skull (max 2 (x `div` 2)) (max 2 x)
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ toChaosTokenValue attrs Tablet 2 3
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 4 6
    otherFace -> getChaosTokenValue iid otherFace attrs

readInvestigatorDefeat :: ReverseQueue m => m ()
readInvestigatorDefeat = do
  defeated <- select DefeatedInvestigator
  unless (null defeated) $ scenarioI18n $ scope "resolutions" do
    resolutionOnly defeated $ scope "investigatorDefeat" $ setTitle "title" >> p "body"
    for_ defeated drivenInsane

{- FOURMOLU_DISABLE -}
standaloneChaosTokens :: [ChaosTokenFace]
standaloneChaosTokens =
  [ PlusOne , Zero , Zero , MinusOne , MinusOne , MinusTwo , MinusTwo , MinusThree , MinusFour
  , Skull , Skull , Cultist , Tablet , ElderThing , AutoFail , ElderSign
  ]
{- FOURMOLU_ENABLE -}

setupBeforeTheBlackThrone :: (HasI18n, ReverseQueue m) => ScenarioAttrs -> ScenarioBuilderT m ()
setupBeforeTheBlackThrone _attrs = do
  setup $ ul do
    li "gatherSets"
    li "setLocationsAside"
    li "placeStart"
    li "cosmos"
    li "placeLocations"
    li "setAside"
    li "placeAzathoth"
    li "thePathWindsBeforeYou"
    unscoped $ li "shuffleRemainder"

  scope "theCosmos" $ flavor $ h "title" >> p "body"
  scope "emptySpace" $ flavor $ h "title" >> p "body"

  pathWindsBeforeYouCount <- getRecordCount ThePathWindsBeforeYou
  whenReturnTo $ gather Set.ReturnToBeforeTheBlackThrone
  gather Set.BeforeTheBlackThrone
  gather Set.AgentsOfAzathoth
  gather Set.InexorableFate `orWhenReturnTo` gather Set.UnspeakableFate
  gather Set.AncientEvils `orWhenReturnTo` gather Set.ImpendingEvils
  gather Set.DarkCult

  cosmicIngress <- place Locations.cosmicIngress
  startAt cosmicIngress

  isReturnTo <- getIsReturnTo
  cosmosCards' <-
    shuffle
      $ [ Locations.infinityOfDarkness
        , Locations.infinityOfDarkness
        , Locations.infinityOfDarkness
        , Locations.cosmicGate
        , Locations.pathwayIntoVoid
        , Locations.pathwayIntoVoid
        , Locations.dancersMist
        , Locations.dancersMist
        , Locations.dancersMist
        , Locations.flightIntoOblivion
        , Locations.flightIntoOblivion
        , Locations.flightIntoOblivion
        ]
      <> (guard isReturnTo *> [Locations.nightmareBreach, Locations.interstellarAbyss, Locations.windingGulf])

  let
    (topCosmosCard, cosmosCards) =
      case cosmosCards' of
        (x : xs) -> (x, xs)
        _ -> error "did not have enough cards"

  (firstCosmosCard, secondCosmosCard) <-
    shuffleM [topCosmosCard, Locations.hideousPalace] <&> \case
      [x, y] -> (x, y)
      _ -> error "did not have enough cards"

  firstCosmos <- place firstCosmosCard
  secondCosmos <- place secondCosmosCard

  lead <- getLead
  (map toCard -> cards, _) <- fieldMap InvestigatorDeck (draw 6) lead
  let
    emptySpaceLocations = [Pos 0 1, Pos 0 (-1), Pos 1 1, Pos 1 0, Pos 1 (-1), Pos 2 0]
    emptySpaces = zip emptySpaceLocations cards

  let cosmos = initCosmos @Card @LocationId

  placeEnemy Enemies.azathoth Global

  pushAll
    $ [ PlaceCosmos lead cosmicIngress (CosmosLocation (Pos 0 0) cosmicIngress)
      , PlaceCosmos lead firstCosmos (CosmosLocation (Pos 2 1) firstCosmos)
      , PlaceCosmos lead secondCosmos (CosmosLocation (Pos 2 (-1)) secondCosmos)
      ]
    <> map (ObtainCard . toCardId) cards

  for_ emptySpaces $ \(pos, card) -> do
    emptySpace' <- placeLocationCard Locations.emptySpace
    push $ PlaceCosmos lead emptySpace' (EmptySpace pos card)

  setAside
    $ [Locations.courtOfTheGreatOldOnes, Locations.theBlackThrone, Enemies.piperOfAzathoth]
    <> (guard isReturnTo *> replicate 4 Assets.nightgauntSteed)
  setAgendaDeck [Agendas.wheelOfFortuneX, Agendas.itAwaits, Agendas.theFinalCountdown]

  let act3 = if isReturnTo then Acts.whatMustBeDoneV2 else Acts.whatMustBeDone
  setActDeck [Acts.theCosmosBeckons, Acts.inAzathothsDomain, act3]
  setLayout $ cosmosToGrid cosmos
  addExtraDeck CosmosDeck cosmosCards
  setMeta cosmos
  setUsesGrid
  placeTokensOnScenarioReference Token.Resource pathWindsBeforeYouCount

instance RunMessage BeforeTheBlackThrone where
  runMessage msg s@(BeforeTheBlackThrone attrs) = runQueueT $ scenarioI18n $ case msg of
    StandaloneSetup -> do
      setChaosTokens standaloneChaosTokens
      pure s
    PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "body"
      pure s
    Setup -> runScenarioSetup BeforeTheBlackThrone attrs $ setupBeforeTheBlackThrone attrs
    EndSetup -> do
      isReturnTo <- Scenario.getIsReturnTo
      pathWindsBeforeYouCount <- getRecordCount ThePathWindsBeforeYou
      when (isReturnTo && pathWindsBeforeYouCount >= 2) do
        leadChooseOneM do
          scenarioI18n $ questionLabeled' "nightgauntSteed"
          withI18n $ labeled' "yes" do
            removeTokens ScenarioSource ScenarioTarget #resource 2
            eachInvestigator $ createAssetAt_ Assets.nightgauntSteed . InPlayArea
          withI18n $ labeled' "no" nothing
        doStep 1 EndSetup
      BeforeTheBlackThrone <$> liftRunMessage msg attrs
    DoStep 1 EndSetup -> do
      getSetAsideCardsMatching (cardIs Assets.nightgauntSteed) >>= traverse_ obtainCard
      pure s
    SetScenarioMeta meta -> do
      case fromJSON @(Cosmos Card LocationId) meta of
        Error err -> error err
        Success cosmos -> pure $ BeforeTheBlackThrone $ attrs & metaL .~ meta & locationLayoutL .~ cosmosToGrid cosmos
    PlaceCosmos {} -> do
      pushAll [Do msg, After msg]
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case chaosTokenFace token of
        Cultist -> findAndDrawEncounterCard iid (CardWithTrait Trait.Cultist)
        Tablet -> do
          azathoth <- selectJust $ IncludeOmnipotent $ enemyIs Enemies.azathoth
          initiateEnemyAttack azathoth Tablet iid
        _ -> pure ()
      pure s
    ResolveChaosToken _ Cultist iid -> do
      drawAnotherChaosToken iid
      pure s
    ResolveChaosToken _ ElderThing _ -> do
      v <- getSkillTestModifiedSkillValue
      azathoth <- selectJust $ IncludeOmnipotent $ enemyIs Enemies.azathoth
      when (v == 0) $ placeDoom ElderThing azathoth 1
      pure s
    ScenarioResolution n -> scope "resolutions" do
      case n of
        NoResolution -> push R1
        Resolution x | x == 1 || x == 11 -> do
          when (x == 1) readInvestigatorDefeat
          resolution "resolution1"
          record AzathothDevouredTheUniverse
          eachInvestigator (kill attrs)
          gameOver
        Resolution 2 -> do
          readInvestigatorDefeat
          resolutionWithXp "resolution2" $ allGainXpWithBonus' attrs $ toBonus "resolution2" 5
          record TheLeadInvestigatorHasJoinedThePipersOfAzathoth
          record AzathothSlumbersForNow
          drivenInsane =<< getLead
          eachInvestigator (`sufferMentalTrauma` 2)
          endOfScenario
        Resolution 3 -> do
          readInvestigatorDefeat
          resolutionWithXp "resolution3" $ allGainXpWithBonus' attrs $ toBonus "resolution3" 5
          record AzathothSlumbersForNow
          eachInvestigator (`sufferPhysicalTrauma` 2)
          endOfScenario
        Resolution 4 -> do
          readInvestigatorDefeat
          storyWithChooseOneM' (compose.resolution $ scope "resolution4" $ setTitle "title" >> p "body")
            $ scenarioI18n do
              labeled' "itMustBeDone" $ push R5
              labeled' "iRefuse" $ push $ ScenarioResolution (Resolution 11) -- actually 1
        Resolution 5 -> do
          readInvestigatorDefeat
          resolutionWithXp "resolution5" $ allGainXpWithBonus' attrs $ toBonus "resolution5" 10
          record AzathothSlumbersForNow
          record TheInvestigatorsSignedTheBlackBookOfAzathoth
          eachInvestigator \iid -> sufferTrauma iid 2 2
          endOfScenario
        Resolution 6 -> do
          readInvestigatorDefeat
          resolutionWithXp "resolution6" $ allGainXpWithBonus' attrs $ toBonus "resolution6" 5
          record AzathothSlumbersForNow
          record TheInvestigatorsSignedTheBlackBookOfAzathoth
          eachInvestigator \iid -> sufferTrauma iid 1 1
          endOfScenario
        _ -> error "unknown resolution"
      pure s
    _ -> BeforeTheBlackThrone <$> liftRunMessage msg attrs
