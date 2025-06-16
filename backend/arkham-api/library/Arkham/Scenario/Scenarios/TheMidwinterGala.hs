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
import Data.Aeson (FromJSON, ToJSON)
import Data.List (delete)
import Arkham.Trait (Trait (Detective, Guest, Innocent, Madness, Police, Private))
import Arkham.Treachery.Cards qualified as Treacheries

data Faction
  = TheFoundation
  | MiskatonicUniversity
  | TheSyndicate
  | SilverTwilightLodge
  deriving stock (Show, Eq, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Meta = Meta {ally :: Faction, rival :: Faction}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype TheMidwinterGala = TheMidwinterGala ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | Basic scenario definition
-- Map layout and scenario code will be added later

theMidwinterGala :: Difficulty -> TheMidwinterGala
theMidwinterGala difficulty =
  sideStory
    TheMidwinterGala
    "00000" -- TODO scenario code
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
      ]

  let
    factionStoryAllied = \case
      TheFoundation -> Stories.tmgTheFoundationAllied
      MiskatonicUniversity -> Stories.tmgMiskatonicUniversityAllied
      TheSyndicate -> Stories.tmgTheSyndicateAllied
      SilverTwilightLodge -> Stories.tmgSilverTwilightLodgeAllied
    factionStoryRival = \case
      TheFoundation -> Stories.tmgTheFoundationRival
      MiskatonicUniversity -> Stories.tmgMiskatonicUniversityRival
      TheSyndicate -> Stories.tmgTheSyndicateRival
      SilverTwilightLodge -> Stories.tmgSilverTwilightLodgeRival

    leaderAsset = \case
      TheFoundation -> Assets.valeriyaAntonovaWantsOutOfHereCard
      MiskatonicUniversity -> Assets.caldwellPhilipsEnthralledByLegendsCard
      TheSyndicate -> Assets.johnnyValoneReadyToMakeADealCard
      SilverTwilightLodge -> Assets.carlSanfordLustingForPowerCard

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
    ScenarioResolution _ -> do
      -- TODO: handle resolutions
      pure s
    _ -> TheMidwinterGala <$> liftRunMessage msg attrs
