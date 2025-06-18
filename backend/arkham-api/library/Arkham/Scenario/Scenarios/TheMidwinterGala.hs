module Arkham.Scenario.Scenarios.TheMidwinterGala (theMidwinterGala) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (AssetController, AssetTraits))
import Arkham.Card.CardDef
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Agenda
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.I18n
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (enemyAt)
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Placement
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Scenario.Import.Lifted hiding (InvestigatorDamage)
import Arkham.Story.Cards qualified as Stories
import Arkham.Trait (Trait (Guest, Leader, Monster, Private))
import Arkham.Treachery.Cards qualified as Treacheries

data Faction
  = TheFoundation
  | MiskatonicUniversity
  | TheSyndicate
  | TheSilverTwilightLodge
  | LocalsOfKingsport
  deriving stock (Show, Eq, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

factionLabel :: HasI18n => Faction -> Text
factionLabel faction = unscoped $ standaloneI18n "theMidwinterGala" $ scope "faction" $ case faction of
  TheFoundation -> "theFoundation"
  MiskatonicUniversity -> "miskatonicUniversity"
  TheSyndicate -> "theSyndicate"
  TheSilverTwilightLodge -> "theSilverTwilightLodge"
  LocalsOfKingsport -> "localsOfKingsport"

data Meta = Meta {ally :: Faction, rival :: Faction}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype TheMidwinterGala = TheMidwinterGala ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Basic scenario definition
Map layout will be added later
-}
theMidwinterGala :: Difficulty -> TheMidwinterGala
theMidwinterGala difficulty =
  sideStory
    TheMidwinterGala
    "71001"
    "The Midwinter Gala"
    difficulty
    [ "secondFloor1   secondFloor2 secondFloor3 ."
    , "lobby          groundFloor1 groundFloor2 groundFloor3"
    , "lanternChamber .           .           ."
    ]

instance HasChaosTokenValue TheMidwinterGala where
  getChaosTokenValue iid tokenFace (TheMidwinterGala attrs) = case tokenFace of
    Skull -> do
      step <- getCurrentAgendaStep
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

setupTheMidwinterGala :: ReverseQueue m => ScenarioAttrs -> ScenarioBuilderT m ()
setupTheMidwinterGala attrs = do
  gather Set.TheMidwinterGala
  setActDeck [Acts.meetAndGreet, Acts.findingTheJewel]
  setAgendaDeck [Agendas.maskedRevelers, Agendas.unexpectedGuests, Agendas.aKillerParty]

  lobby <- place Locations.lobbyTheMidwinterGala
  lanternChamber <- place Locations.lanternChamber
  groundFloors <-
    placeGroupCapture "groundFloor"
      =<< shuffle
        [ Locations.artGalleryTheMidwinterGala
        , Locations.ballroomTheMidwinterGala
        , Locations.barroom
        ]

  setAside
    =<< shuffle
      [ Locations.bedroomTheMidwinterGala
      , Locations.libraryTheMidwinterGala
      , Locations.parlorTheMidwinterGala
      ]

  startAt lobby

  let Meta {ally, rival} = toResult attrs.meta

  let
    factionStoryAllied = \case
      TheFoundation -> Stories.theFoundationAllied
      MiskatonicUniversity -> Stories.miskatonicUniversityAllied
      TheSyndicate -> Stories.theSyndicateAllied
      TheSilverTwilightLodge -> Stories.silverTwilightLodgeAllied
      LocalsOfKingsport -> Stories.localsOfKingsportAllied

    factionStoryRival = \case
      TheFoundation -> Stories.theFoundationRival
      MiskatonicUniversity -> Stories.miskatonicUniversityRival
      TheSyndicate -> Stories.theSyndicateRival
      TheSilverTwilightLodge -> Stories.silverTwilightLodgeRival
      LocalsOfKingsport -> Stories.localsOfKingsportRival

    factionRivalCard = \case
      TheFoundation -> Enemies.rookieCop
      MiskatonicUniversity -> Treacheries.confusion
      TheSyndicate -> Treacheries.coldStreak
      TheSilverTwilightLodge -> Treacheries.wardOfPreservation
      LocalsOfKingsport -> Treacheries.unlucky

    leaderAsset = \case
      TheFoundation -> Assets.valeriyaAntonovaWantsOutOfHere
      MiskatonicUniversity -> Assets.caldwellPhilipsEnthralledByLegends
      TheSyndicate -> Assets.johnnyValoneReadyToMakeADeal
      TheSilverTwilightLodge -> Assets.carlSanfordLustingForPower
      LocalsOfKingsport -> Assets.williamBainLookingForThoseLost

    factionGuests = \case
      TheFoundation ->
        [ Assets.archibaldHudson
        , Assets.specialAgentCallahan
        , Assets.horacioMartinez
        ]
      MiskatonicUniversity ->
        [ Assets.drMyaBadry
        , Assets.lucasTetlow
        , Assets.elizabethConrad
        ]
      TheSyndicate ->
        [ Assets.mirandaKeeper
        , Assets.arseneRenard
        , Assets.novaMalone
        ]
      TheSilverTwilightLodge ->
        [ Assets.prudenceDouglas
        , Assets.sarahVanShaw
        , Assets.raymondLoggins
        ]
      LocalsOfKingsport ->
        [ Assets.deloresGadling
        , Assets.thomasOlney
        , Assets.claireWilson
        ]

  setAside [factionStoryAllied ally]
  removeEvery [factionRivalCard ally]

  lead <- getLead
  beginWithStoryAsset lead (leaderAsset ally)

  shuffledGuests <- shuffle (factionGuests ally)
  for_ (zip shuffledGuests groundFloors) (uncurry assetAt_)

  setAside [factionStoryRival rival, leaderAsset rival, factionRivalCard rival]
  removeEvery $ factionGuests rival

  let otherFactions = filter (`notElem` [ally, rival]) [minBound ..]

  for_ otherFactions \faction ->
    removeEvery [factionStoryAllied faction, factionRivalCard faction, leaderAsset faction]

  (inPlayGuests, guestDeck) <- splitAt 3 <$> shuffle (concatMap factionGuests otherFactions)
  addExtraDeck GuestDeck =<< shuffle guestDeck
  for_ (zip inPlayGuests groundFloors) (uncurry assetAt_)

  theBloodlessMan <- enemyAt Enemies.theBloodlessMan lanternChamber
  placeAsset_ Assets.thePaleLanternHypnoticGlow (AttachedToEnemy theBloodlessMan)
  exhaustThis theBloodlessMan

  monsters <- asDefs <$> amongGathered (CardWithTrait Monster)
  setAside
    $ monsters
    <> replicate 2 Treacheries.viciousAmbush
    <> [Enemies.declanPearce, Assets.jewelOfSarnath]

-- calculateScore :: HasGame m => ScenarioAttrs -> Meta -> m Int
-- calculateScore attrs Meta {ally, rival} = do
--   manorNoClue <-
--     selectCount
--       $ LocationWithTrait Manor
--       <> RevealedLocation
--       <> LocationWithoutClues
--   guestControlled <- selectCount $ AssetWithTrait Guest <> AssetControlledBy Anyone
--   spellboundInPlay <- selectAny $ AssetWithModifier (ScenarioModifier "spellbound")
--   agendaId <- getCurrentAgenda
--   Sequence step side <- field AgendaSequence agendaId
--   bloodless <- inVictoryDisplay $ cardIs Enemies.theBloodlessMan
--   bloodlessUnleashed <- inVictoryDisplay $ cardIs Enemies.theBloodlessManUnleashed
--   paleLantern <- inVictoryDisplay $ cardIs Assets.thePaleLanternBeguilingAura
--   declan <- inVictoryDisplay $ cardIs Enemies.declanPearce
--   let
--     factionStoryR = \case
--       TheFoundation -> Stories.theFoundationRival
--       MiskatonicUniversity -> Stories.miskatonicUniversityRival
--       TheSyndicate -> Stories.theSyndicateRival
--       SilverTwilightLodge -> Stories.silverTwilightLodgeRival
--       LocalsOfKingsport -> Stories.localsOfKingsportRival
--   rivalInVictory <- inVictoryDisplay $ cardIs (factionStoryR rival)
--   investigators <- allInvestigators
--   totalDamage <- sum <$> traverse (field InvestigatorDamage) investigators
--   totalHorror <- sum <$> traverse (field InvestigatorHorror) investigators
--   defeatedAny <- selectAny DefeatedInvestigator
--   playerCount <- getPlayerCount
--   let
--     damageBonus =
--       if not defeatedAny && totalDamage <= playerCount * 2
--         then 3
--         else 0
--     horrorBonus =
--       if not defeatedAny && totalHorror <= playerCount * 2
--         then 3
--         else 0
--     agendaBonus = if side == A && step <= 2 then 5 else 0
--     difficultyBonus = case attrs.difficulty of
--       Hard -> 3
--       Expert -> 6
--       _ -> 0
--     alliedScore = 0
--   pure
--     $ manorNoClue
--     + guestControlled
--     + (if not spellboundInPlay then 4 else 0)
--     + agendaBonus
--     + (if bloodless || bloodlessUnleashed then 4 else 0)
--     + (if paleLantern then 3 else 0)
--     + (if declan then 3 else 0)
--     + (if rivalInVictory then 4 else 0)
--     + damageBonus
--     + horrorBonus
--     + difficultyBonus
--     + alliedScore

instance RunMessage TheMidwinterGala where
  runMessage msg s@(TheMidwinterGala attrs) = runQueueT $ standaloneI18n "theMidwinterGala" $ case msg of
    PreScenarioSetup -> scope "intro" do
      flavor $ h "title" >> p "flavor"
      flavor $ h "title" >> p "body"
      storyWithChooseOneM' (h "title" >> p "guestChoice") do
        for_ (eachWithRest [minBound ..]) \(faction, rest) -> do
          rival <- maybe (error "empty") sample $ nonEmpty rest
          popScope $ labeled' (factionLabel faction) do
            scope "intro" $ flavor $ h "title" >> p (factionLabel faction)
            push $ SetScenarioMeta $ toJSON $ Meta {ally = faction, rival = rival}
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
    ScenarioResolution _r -> do
      -- resigned <- selectAny ResignedInvestigator
      -- case r of
      --   NoResolution ->
      --     story $ if resigned then noResolutionResigned else noResolution
      --   Resolution 1 -> story resolution1
      --   _ -> pure ()

      -- guests <- select $ AssetWithTrait Guest <> AssetControlledBy Anyone
      -- when (notNull guests) do
      --   lead <- getLead
      --   chooseOrRunOne
      --     lead
      --     [ targetLabel
      --         guest
      --         [ do
      --             card <- field AssetCard guest
      --             investigatorIds <- allInvestigators
      --             chooseOrRunOne
      --               lead
      --               [ InvestigatorLabel iid [push $ AddCardToDeckForCampaign iid card]
      --               | iid <- investigatorIds
      --               ]
      --         ]
      --     | guest <- guests
      --     ]

      -- metaValue <- scenarioField ScenarioMeta
      -- let Success (Meta allyFaction _) = fromJSON metaValue
      -- case allyFaction of
      --   TheFoundation -> story resolution2
      --   MiskatonicUniversity -> story resolution3
      --   TheSyndicate -> story resolution4
      --   SilverTwilightLodge -> story resolution5
      --   LocalsOfKingsport -> story resolution6

      -- story resolution7
      -- metaValue <- scenarioField ScenarioMeta
      -- let Success meta = fromJSON metaValue
      -- score <- calculateScore attrs meta
      -- let bonus
      --       | score >= 50 = 5
      --       | score >= 40 = 4
      --       | score >= 30 = 3
      --       | score >= 20 = 2
      --       | score >= 10 = 1
      --       | otherwise = 0
      -- _xp <- allGainXpWithBonus' attrs (toBonus "bonus.score" bonus)
      -- case r of
      --   NoResolution | not resigned -> record TheInvestigatorsWereDefeatedAtTheMidwinterGala
      --   _ -> record TheInvestigatorsSurvivedTheMidwinterGala
      -- endOfScenario
      pure s
    ForTarget (AssetTarget aid) (ScenarioSpecific "spellbound" _) -> do
      traits <- field AssetTraits aid
      if Leader `member` traits
        then removeFromGame aid
        else do
          mController <- field AssetController aid
          lead <- getLead
          let iid = fromMaybe lead mController
          push $ LoseControlOfAsset aid
          push $ HealAllDamageAndHorror (AssetTarget aid) GameSource
          gameModifier ScenarioSource aid (ScenarioModifier "spellbound")
          flipOver iid aid
      pure s
    _ -> TheMidwinterGala <$> liftRunMessage msg attrs
