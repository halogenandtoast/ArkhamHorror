module Arkham.Scenario.Scenarios.TheMidwinterGala (theMidwinterGala) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Agenda.Sequence
import Arkham.Agenda.Types (Field(AgendaSequence))
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Classes
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers hiding (skillTestModifier)
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Message.Lifted.Move
import Arkham.Message.Lifted (allGainXpWithBonus')
import Arkham.Placement
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Types (ScenarioAttrs(..))
import Arkham.ScenarioLogKey
import Arkham.Scenarios.TheMidwinterGala.FlavorText
import Arkham.Scenarios.TheMidwinterGala.Helpers
import Arkham.Story.Cards qualified as Stories
import Data.Aeson (FromJSON, ToJSON, Result(..), fromJSON, toJSON)
import Data.List (delete)
import Arkham.Trait (Trait (Detective, Guest, Innocent, Madness, Police, Private))
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Helpers.Xp (toBonus)

data Faction
  = TheFoundation
  | MiskatonicUniversity
  | TheSyndicate
  | SilverTwilightLodge
  | LocalsOfKingsport
  deriving stock (Show, Eq, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Meta = Meta {ally :: Faction, rival :: Faction}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype TheMidwinterGala = TheMidwinterGala ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | Basic scenario definition
-- Map layout will be added later

theMidwinterGala :: Difficulty -> TheMidwinterGala
theMidwinterGala difficulty =
  sideStory
    TheMidwinterGala
    "71001"
    "The Midwinter Gala"
    difficulty
    [ "secondFloor1 secondFloor2 secondFloor3 ."
    , "lobby       groundFloor1 groundFloor2 groundFloor3"
    , "lanternRoom .           .           ."
    ]
    (metaL .~ toJSON (Meta TheFoundation TheSyndicate))

instance HasChaosTokenValue TheMidwinterGala where
  getChaosTokenValue iid tokenFace (TheMidwinterGala attrs) = case tokenFace of
    Skull -> do
      agendaId <- selectJust AnyAgenda
      step <- fieldMap AgendaSequence (unAgendaStep . agendaStep) agendaId
      let n = if isEasyStandard attrs then step else step + 1
      pure $ ChaosTokenValue Skull (NegativeModifier n)
    Cultist -> do
      n <-
        if isEasyStandard attrs
          then do
            guests <- selectCount $ AssetWithTrait Guest <> assetAtLocationWith iid
            pure $ min 5 guests
          else
            selectCount $ StoryAsset <> assetAtLocationWith iid
      pure $ ChaosTokenValue Cultist (NegativeModifier n)
    Tablet -> do
      atPrivateLocation <-
        selectAny $ locationWithInvestigator iid <> LocationWithTrait Private
      let base = if isEasyStandard attrs then 2 else 3
          privatePenalty = if isEasyStandard attrs then 4 else 5
          n = if atPrivateLocation then privatePenalty else base
      pure $ ChaosTokenValue Tablet (NegativeModifier n)
    ElderThing ->
      pure $ ChaosTokenValue ElderThing (NegativeModifier $ if isEasyStandard attrs then 3 else 4)
    otherFace -> getChaosTokenValue iid otherFace attrs

setupTheMidwinterGala :: (HasGame m, MonadRandom m, ReverseQueue m) => ScenarioAttrs -> ScenarioBuilderT m ()
setupTheMidwinterGala attrs = do
  gather Set.TheMidwinterGala
  setActDeck [Acts.meetAndGreet, Acts.findingTheJewel]
  setAgendaDeck [Agendas.maskedRevelers, Agendas.unexpectedGuests, Agendas.aKillerParty]

  lobby <- place Locations.tmgLobby
  lantern <- place Locations.tmgLanternChamber

  groundFloors <- placeGroupCapture "groundFloor" =<< shuffle
    [ Locations.tmgArtGallery
    , Locations.tmgBallroom
    , Locations.tmgBarroom
    ]

  setAside =<< shuffle
    [ Locations.tmgBedroom
    , Locations.tmgLibrary
    , Locations.tmgParlor
    ]

  startAt lobby

  lead <- getLead
  allyChoice <-
    chooseOne lead
      [ Label "The Foundation" $ pure TheFoundation
      , Label "Miskatonic University" $ pure MiskatonicUniversity
      , Label "The Syndicate" $ pure TheSyndicate
      , Label "Silver Twilight Lodge" $ pure SilverTwilightLodge
      , Label "Locals of Kingsport" $ pure LocalsOfKingsport
      ]

  let
    factionStoryAllied = \case
      TheFoundation -> Stories.tmgTheFoundationAllied
      MiskatonicUniversity -> Stories.tmgMiskatonicUniversityAllied
      TheSyndicate -> Stories.tmgTheSyndicateAllied
      SilverTwilightLodge -> Stories.tmgSilverTwilightLodgeAllied
      LocalsOfKingsport -> Stories.tmgLocalsOfKingsportAllied
    factionStoryRival = \case
      TheFoundation -> Stories.tmgTheFoundationRival
      MiskatonicUniversity -> Stories.tmgMiskatonicUniversityRival
      TheSyndicate -> Stories.tmgTheSyndicateRival
      SilverTwilightLodge -> Stories.tmgSilverTwilightLodgeRival
      LocalsOfKingsport -> Stories.tmgLocalsOfKingsportRival

    leaderAsset = \case
      TheFoundation -> Assets.valeriyaAntonovaWantsOutOfHereCard
      MiskatonicUniversity -> Assets.caldwellPhilipsEnthralledByLegendsCard
      TheSyndicate -> Assets.johnnyValoneReadyToMakeADealCard
      SilverTwilightLodge -> Assets.carlSanfordLustingForPowerCard
      LocalsOfKingsport -> Assets.williamBainLookingForThoseLostCard

    factionGuests = \case
      TheFoundation ->
        [ Assets.archibaldHudsonCard
        , Assets.specialAgentCallahanCard
        , Assets.horacioMartinezCard
        ]
      MiskatonicUniversity ->
        [ Assets.drMyaBadryCard
        , Assets.lucasTetlowCard
        , Assets.elizabethConradCard
        ]
      TheSyndicate ->
        [ Assets.mirandaKeeperCard
        , Assets.arseneRenardCard
        , Assets.novaMaloneCard
        ]
      SilverTwilightLodge ->
        [ Assets.prudenceDouglasCard
        , Assets.sarahVanShawCard
        , Assets.raymondLogginsCard
        ]
      LocalsOfKingsport ->
        [ Assets.deloresGadlingCard
        , Assets.thomasOlneyCard
        , Assets.claireWilsonCard
        ]

  setAside [factionStoryAllied allyChoice]
  removeEvery [factionStoryRival allyChoice]

  beginWithStoryAsset lead (leaderAsset allyChoice)

  shuffledGuests <- shuffle (factionGuests allyChoice)
  for_ (zip shuffledGuests groundFloors) \(g, lid) -> assetAt_ g lid

  let remainingFactions = delete allyChoice [minBound .. maxBound]
  rival <- sample remainingFactions
  setAside [factionStoryRival rival, leaderAsset rival]
  removeEvery (factionGuests rival)

  let otherFactions = delete rival remainingFactions
  for_ otherFactions \f -> do
    removeEvery [factionStoryAllied f, factionStoryRival f]
    removeEvery (leaderAsset f : factionGuests f)

  let guestCards = concatMap factionGuests otherFactions
  addExtraDeck GuestDeck =<< shuffle guestCards
  drawn <- take 3 <$> getGuestDeck
  for_ (zip drawn groundFloors) \(c, lid) -> createAssetAt_ c (AtLocation lid)

  enemyId <- enemyAt Enemies.theBloodlessMan lantern
  pale <- genCard Assets.thePaleLanternHypnoticGlowCard
  createAssetAt_ pale (AttachedToEnemy enemyId)
  push $ Exhaust (EnemyTarget enemyId)

  setAside
    $ replicate 2 Treacheries.viciousAmbush
    <> [Enemies.declanPearce, Assets.jewelOfSarnathCard]

  setMeta $ Meta {ally = allyChoice, rival = rival}

calculateScore :: HasGame m => ScenarioAttrs -> Meta -> m Int
calculateScore attrs Meta {ally, rival} = do
  manorNoClue <-
    selectCount
      $ LocationWithTrait Manor
      <> RevealedLocation
      <> LocationWithoutClues
  guestControlled <- selectCount $ AssetWithTrait Guest <> AssetControlledBy Anyone
  spellboundInPlay <-
    selectAny $ AssetWithMetaKeyValue "spellbound" (toJSON True)
  agendaId <- getCurrentAgenda
  AgendaSequence step side <- field AgendaSequence agendaId
  bloodless <- inVictoryDisplay $ cardIs Enemies.theBloodlessMan
  bloodlessUnleashed <- inVictoryDisplay $ cardIs Enemies.theBloodlessManUnleashed
  paleLantern <- inVictoryDisplay $ cardIs Assets.thePaleLanternBeguilingAuraCard
  declan <- inVictoryDisplay $ cardIs Enemies.declanPearce
  let
    factionStoryR = \case
      TheFoundation -> Stories.tmgTheFoundationRival
      MiskatonicUniversity -> Stories.tmgMiskatonicUniversityRival
      TheSyndicate -> Stories.tmgTheSyndicateRival
      SilverTwilightLodge -> Stories.tmgSilverTwilightLodgeRival
      LocalsOfKingsport -> Stories.tmgLocalsOfKingsportRival
  rivalInVictory <- inVictoryDisplay $ cardIs (factionStoryR rival)
  investigators <- allInvestigators
  totalDamage <- sum <$> traverse (field InvestigatorDamage) investigators
  totalHorror <- sum <$> traverse (field InvestigatorHorror) investigators
  defeatedAny <- selectAny DefeatedInvestigator
  playerCount <- getPlayerCount
  let
    damageBonus =
      if not defeatedAny && totalDamage <= playerCount * 2
        then 3
        else 0
    horrorBonus =
      if not defeatedAny && totalHorror <= playerCount * 2
        then 3
        else 0
    agendaBonus = if side == A && step <= 2 then 5 else 0
    difficultyBonus = case attrs.difficulty of
      Hard -> 3
      Expert -> 6
      _ -> 0
    alliedScore = 0
  pure
    $ manorNoClue
    + guestControlled
    + (if not spellboundInPlay then 4 else 0)
    + agendaBonus
    + (if bloodless || bloodlessUnleashed then 4 else 0)
    + (if paleLantern then 3 else 0)
    + (if declan then 3 else 0)
    + (if rivalInVictory then 4 else 0)
    + damageBonus
    + horrorBonus
    + difficultyBonus
    + alliedScore
instance RunMessage TheMidwinterGala where
  runMessage msg s@(TheMidwinterGala attrs) = runQueueT $ case msg of
    PreScenarioSetup -> do
      story intro1
      pure s
    Setup -> runScenarioSetup TheMidwinterGala attrs $ setupTheMidwinterGala attrs
    StandaloneSetup -> do
      let
        standardTokens =
          [ PlusOne
          , Zero
          , Zero
          , MinusOne
          , MinusOne
          , MinusOne
          , MinusTwo
          , MinusTwo
          , MinusThree
          , MinusFour
          , Skull
          , Skull
          , Cultist
          , Tablet
          , ElderThing
          , AutoFail
          , ElderSign
          ]
        hardTokens =
          [ Zero
          , Zero
          , MinusOne
          , MinusOne
          , MinusOne
          , MinusTwo
          , MinusThree
          , MinusFour
          , MinusSix
          , Skull
          , Skull
          , Cultist
          , Tablet
          , ElderThing
          , ElderThing
          , AutoFail
          , ElderSign
          ]
        expertTokens =
          [ Zero
          , MinusOne
          , MinusOne
          , MinusTwo
          , MinusThree
          , MinusFour
          , MinusFive
          , MinusSix
          , MinusEight
          , Skull
          , Skull
          , Cultist
          , Tablet
          , ElderThing
          , ElderThing
          , AutoFail
          , ElderSign
          ]
        tokens = case attrs.difficulty of
          Easy -> standardTokens
          Standard -> standardTokens
          Hard -> hardTokens
          Expert -> expertTokens
      setChaosTokens tokens
      pure s
    ScenarioResolution r -> scope "resolutions" do
      resigned <- selectAny ResignedInvestigator
      case r of
        NoResolution ->
          story $ if resigned then noResolutionResigned else noResolution
        Resolution 1 -> story resolution1
        _ -> pure ()

      guests <- select $ AssetWithTrait Guest <> AssetControlledBy Anyone
      when (notNull guests) do
        lead <- getLead
        chooseOrRunOne lead
          [ targetLabel guest [ do
              card <- field AssetCard guest
              investigatorIds <- allInvestigators
              chooseOrRunOne lead
                [ InvestigatorLabel iid [push $ AddCardToDeckForCampaign iid card]
                | iid <- investigatorIds
                ]
            ]
          | guest <- guests
          ]

      metaValue <- scenarioField ScenarioMeta
      let Success (Meta allyFaction _) = fromJSON metaValue
      case allyFaction of
        TheFoundation -> story resolution2
        MiskatonicUniversity -> story resolution3
        TheSyndicate -> story resolution4
        SilverTwilightLodge -> story resolution5
        LocalsOfKingsport -> story resolution6

      story resolution7
      metaValue <- scenarioField ScenarioMeta
      let Success meta = fromJSON metaValue
      score <- calculateScore attrs meta
      let bonus
            | score >= 50 = 5
            | score >= 40 = 4
            | score >= 30 = 3
            | score >= 20 = 2
            | score >= 10 = 1
            | otherwise = 0
      _xp <- allGainXpWithBonus' attrs (toBonus "bonus.score" bonus)
      case r of
        NoResolution | not resigned -> record TheInvestigatorsWereDefeatedAtTheMidwinterGala
        _ -> record TheInvestigatorsSurvivedTheMidwinterGala
      endOfScenario
      pure s
    _ -> TheMidwinterGala <$> liftRunMessage msg attrs
