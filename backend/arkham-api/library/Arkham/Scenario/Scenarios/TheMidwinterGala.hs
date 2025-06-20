module Arkham.Scenario.Scenarios.TheMidwinterGala (theMidwinterGala) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Agenda.Sequence
import Arkham.Agenda.Types (Field (AgendaSequence))
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types qualified as Field
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Deck qualified as Deck
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Creation (createExhausted)
import Arkham.Exception
import Arkham.Helpers
import Arkham.Helpers.Agenda
import Arkham.Helpers.FlavorText
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Helpers.Xp
import Arkham.I18n
import Arkham.Investigator.Types (Field (InvestigatorDamage, InvestigatorHorror))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (enemyAt)
import Arkham.Message (StoryMode (..))
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement qualified as Placement
import Arkham.Placement
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.Scenario.Import.Lifted hiding (InvestigatorDamage)
import Arkham.Scenarios.TheMidwinterGala.Faction
import Arkham.Scenarios.TheMidwinterGala.Helpers
import Arkham.Scenarios.TheMidwinterGala.Meta
import Arkham.Story.Cards qualified as Stories
import Arkham.Trait (Trait (Guest, Leader, Manor, Monster, Private, SecondFloor))
import Arkham.Treachery.Cards qualified as Treacheries
import Data.Map.Strict qualified as Map

{- FOURMOLU_DISABLE -}
standardTokens, hardTokens, expertTokens :: [ChaosTokenFace]
standardTokens =
  [ PlusOne , Zero , Zero , MinusOne , MinusOne , MinusOne , MinusTwo , MinusTwo , MinusThree
  , MinusFour , Skull , Skull , Cultist , Tablet , ElderThing , AutoFail , ElderSign
  ]
hardTokens =
  [ Zero , Zero , MinusOne , MinusOne , MinusOne , MinusTwo , MinusThree , MinusFour , MinusSix
  , Skull , Skull , Cultist , Tablet , ElderThing , ElderThing , AutoFail , ElderSign
  ]
expertTokens =
  [ Zero , MinusOne , MinusOne , MinusTwo , MinusThree , MinusFour , MinusFive , MinusSix
  , MinusEight , Skull , Skull , Cultist , Tablet , ElderThing , ElderThing , AutoFail , ElderSign
  ]
{- FOURMOLU_ENABLE -}

newtype TheMidwinterGala = TheMidwinterGala ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

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
          then min 5 <$> selectCount (AssetWithTrait Guest <> assetAtLocationWith iid)
          else selectCount $ StoryAsset <> assetAtLocationWith iid
      pure $ ChaosTokenValue Cultist (NegativeModifier n)
    Tablet -> do
      atPrivateLocation <- selectAny $ locationWithInvestigator iid <> LocationWithTrait Private
      let n
            | atPrivateLocation = if isEasyStandard attrs then 4 else 5
            | otherwise = if isEasyStandard attrs then 2 else 3
      pure $ ChaosTokenValue Tablet (NegativeModifier n)
    ElderThing ->
      pure $ ChaosTokenValue ElderThing (NegativeModifier $ if isEasyStandard attrs then 3 else 4)
    otherFace -> getChaosTokenValue iid otherFace attrs

calculateScore :: HasGame m => ScenarioAttrs -> m (Map Tally Int)
calculateScore attrs = do
  let Meta {rival} = toResult attrs.meta
  manorNoClue <-
    selectCount
      $ LocationWithTrait Manor
      <> RevealedLocation
      <> LocationWithoutClues
  guestControlled <- selectCount $ AssetWithTrait Guest <> AssetControlledBy Anyone
  spellboundInPlay <- selectAny (SpellboundAsset AnyAsset)
  agendaId <- getCurrentAgenda
  Sequence step side <- field AgendaSequence agendaId
  bloodless <- inVictoryDisplay $ cardIs Enemies.theBloodlessMan
  bloodlessUnleashed <- inVictoryDisplay $ cardIs Enemies.theBloodlessManUnleashed
  paleLantern <- inVictoryDisplay $ cardIs Assets.thePaleLanternBeguilingAura
  declan <- inVictoryDisplay $ cardIs Enemies.declanPearce
  let
    factionStoryR = \case
      TheFoundation -> Stories.theFoundationRival
      MiskatonicUniversity -> Stories.miskatonicUniversityRival
      TheSyndicate -> Stories.theSyndicateRival
      TheSilverTwilightLodge -> Stories.silverTwilightLodgeRival
      LocalsOfKingsport -> Stories.localsOfKingsportRival
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
    alliedScore = 0
  pure
    $ mapFromList
      [ (ManorsWithNoClues, manorNoClue)
      , (GuestAssetsControlled, guestControlled)
      , (NoSpellboundInPlay, if not spellboundInPlay then 4 else 0)
      , (Agenda1AOr2A, if side == A && step <= 2 then 5 else 0)
      , (BloodlessManInVictory, if bloodless || bloodlessUnleashed then 4 else 0)
      , (PaleLanternInVictory, if paleLantern then 3 else 0)
      , (DeclanPearceInVictory, if declan then 3 else 0)
      , (RivalInVictory, if rivalInVictory then 3 else 0)
      , (DamageBonus, damageBonus)
      , (HorrorBonus, horrorBonus)
      , (HardMode, if attrs.difficulty == Hard then 3 else 0)
      , (ExpertMode, if attrs.difficulty == Expert then 6 else 0)
      , (FactionBonus, alliedScore)
      ]

instance RunMessage TheMidwinterGala where
  runMessage msg s@(TheMidwinterGala attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      flavor $ h "title" >> p "flavor"
      flavor $ h "title" >> p "body"
      storyWithChooseOneM' (h "title" >> p "guestChoice") do
        for_ (eachWithRest [minBound ..]) \(faction, rest) -> do
          rival <- maybe (error "empty") sample $ nonEmpty rest
          popScope $ labeled' (factionLabel faction) do
            scope "intro" $ flavor $ h "title" >> p (factionLabel faction)
            push $ SetScenarioMeta $ toJSON $ Meta {ally = faction, rival = rival, score = mempty}
      pure s
    Setup -> runScenarioSetup TheMidwinterGala attrs do
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

        leaderEnemy = \case
          TheFoundation -> Enemies.valeriyaAntonovaDontMessWithHer
          MiskatonicUniversity -> Enemies.caldwellPhilipsCompelledByDreams
          TheSyndicate -> Enemies.johnnyValoneHereToCollect
          TheSilverTwilightLodge -> Enemies.carlSanfordDeathlessFanatic
          LocalsOfKingsport -> Enemies.williamBainDefiantToTheLast

        factionGuests = \case
          TheFoundation -> [Assets.archibaldHudson, Assets.specialAgentCallahan, Assets.horacioMartinez]
          MiskatonicUniversity -> [Assets.drMyaBadry, Assets.lucasTetlow, Assets.elizabethConrad]
          TheSyndicate -> [Assets.mirandaKeeper, Assets.arseneRenard, Assets.novaMalone]
          TheSilverTwilightLodge -> [Assets.prudenceDouglas, Assets.sarahVanShaw, Assets.raymondLoggins]
          LocalsOfKingsport -> [Assets.deloresGadling, Assets.thomasOlney, Assets.claireWilson]

      setAside [factionStoryAllied ally]
      removeEvery [factionRivalCard ally]

      lead <- getLead
      beginWithStoryAsset lead (leaderAsset ally)

      shuffledGuests <- shuffle (factionGuests ally)
      for_ (zip shuffledGuests groundFloors) (uncurry assetAt_)

      setAside [factionStoryRival rival, leaderEnemy rival, factionRivalCard rival]
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

      monsters <-
        asDefs <$> amongGathered (CardWithTrait Monster <> not_ (cardIs Enemies.theBloodlessManUnleashed))

      setAside
        $ monsters
        <> replicate 2 Treacheries.viciousAmbush
        <> [Enemies.declanPearce, Assets.jewelOfSarnath]
    StandaloneSetup -> do
      let
        tokens = case attrs.difficulty of
          Easy -> standardTokens
          Standard -> standardTokens
          Hard -> hardTokens
          Expert -> expertTokens
      setChaosTokens tokens
      pure s
    ResolveChaosToken _ ElderThing iid -> do
      ok <-
        selectAny
          $ mapOneOf enemyIs [Enemies.theBloodlessMan, Enemies.theBloodlessManUnleashed]
          <> enemyAtLocationWith iid
      when ok $ assignHorror iid ElderThing 1
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      when (token.face == ElderThing) do
        -- N.B. We may need to track this if the bloodless man can move somehow
        -- between resolving the token and failing the test, as this is an or
        -- and should only happen once. Maybe use the Semaphore?
        ok <-
          selectNone
            $ mapOneOf enemyIs [Enemies.theBloodlessMan, Enemies.theBloodlessManUnleashed]
            <> enemyAtLocationWith iid
        when ok $ assignHorror iid ElderThing 1
      pure s
    ScenarioResolution _r -> do
      let Meta {ally, rival} = toResult attrs.meta
      score <- calculateScore attrs
      push $ SetScenarioMeta $ toJSON $ Meta {ally, rival, score}
      do_ msg
      pure s
    Do (ScenarioResolution r) -> scope "resolutions" do
      case r of
        NoResolution -> do
          resolution "noResolutions" >> do_ R7
        Resolution 1 -> do
          let Meta {ally} = toResult attrs.meta
          storyBuild do
            h "resolution1.title"
            p "resolution1.body"
            ul do
              li "chooseGuest"
              li "jewelOfSarnath"
              for_ [minBound ..] \faction -> li.validate (ally == faction) (unpack $ factionLabel faction)
          eachInvestigator (`forTarget` msg)
          do_ $ case ally of
            TheFoundation -> R2
            MiskatonicUniversity -> R3
            TheSyndicate -> R4
            TheSilverTwilightLodge -> R5
            LocalsOfKingsport -> R6
        Resolution 2 -> do
          resolution "resolution2"
          do_ R7
        Resolution 3 -> do
          resolution "resolution3"
          do_ R7
        Resolution 4 -> do
          resolution "resolution4"
          do_ R7
        Resolution 5 -> do
          resolution "resolution5"
          do_ R7
        Resolution 6 -> do
          resolution "resolution6"
          do_ R7
        Resolution 7 -> do
          let Meta {score} = toResult attrs.meta
          let talliedScore = sum $ Map.elems score
          let
            bonus
              | talliedScore >= 50 = 5
              | talliedScore >= 40 = 4
              | talliedScore >= 30 = 3
              | talliedScore >= 20 = 2
              | talliedScore >= 10 = 1
              | otherwise = 0

          withVars
            [ "bonus1" .= String (if bonus == 1 then "valid" else "invalid")
            , "bonus2" .= String (if bonus == 2 then "valid" else "invalid")
            , "bonus3" .= String (if bonus == 3 then "valid" else "invalid")
            , "bonus4" .= String (if bonus == 4 then "valid" else "invalid")
            , "bonus5" .= String (if bonus == 5 then "valid" else "invalid")
            , "score" .= toJSON talliedScore
            ]
            $ resolutionWithXp "resolution7"
            $ allGainXpWithBonus' attrs (toBonus "bonus.score" bonus)

          storyBuild $ scope "tasks" do
            h "title"
            p "body"
            countVar (findWithDefault 0 ManorsWithNoClues score) $ p "manorsWithNoClues"
            countVar (findWithDefault 0 GuestAssetsControlled score) $ p "guestAssetsControlled"
            countVar (findWithDefault 0 NoSpellboundInPlay score) $ p "noSpellboundInPlay"
            countVar (findWithDefault 0 Agenda1AOr2A score) $ p "agenda1AOr2A"
            countVar (findWithDefault 0 BloodlessManInVictory score) $ p "bloodlessManInVictory"
            countVar (findWithDefault 0 PaleLanternInVictory score) $ p "paleLanternInVictory"
            countVar (findWithDefault 0 DeclanPearceInVictory score) $ p "declanPearceInVictory"
            countVar (findWithDefault 0 RivalInVictory score) $ p "rivalInVictory"
            countVar (findWithDefault 0 DamageBonus score) $ p "damageBonus"
            countVar (findWithDefault 0 HorrorBonus score) $ p "horrorBonus"
            countVar (findWithDefault 0 HardMode score) $ p "hardMode"
            countVar (findWithDefault 0 ExpertMode score) $ p "expertMode"
            countVar (findWithDefault 0 FactionBonus score) $ p "factionBonus"
            countVar talliedScore $ p "total"

          endOfScenario
        _ -> throw $ UnknownResolution r
      pure s
    ForTarget (InvestigatorTarget iid) (ScenarioResolution (Resolution 1)) -> scope "resolutions" do
      guests <- selectWithField Field.AssetCard $ AssetWithTrait Guest <> AssetControlledBy Anyone
      when (notNull guests) do
        chooseOneM iid do
          questionLabeled' "chooseGuest"
          unscoped $ labeled' "skip" nothing
          for_ guests \(guest, card) -> do
            targeting guest do
              removeFromGame guest
              addCampaignCardToDeck iid ShuffleIn card
      pure s
    ForTarget (AssetTarget aid) (ScenarioSpecific "spellbound" _) -> do
      traits <- field Field.AssetTraits aid
      if Leader `member` traits
        then removeFromGame aid
        else do
          cancelAssetLeavePlay aid
          mController <- field Field.AssetController aid
          lead <- getLead
          let iid = fromMaybe lead mController
          loseControlOfAsset aid
          getLocationOf aid >>= \case
            Nothing -> toDiscard ScenarioSource aid
            Just loc -> do
              Placement.place aid (AtLocation loc)
              healAllDamageAndHorror GameSource aid
              flipOverBy iid ScenarioSource aid
      pure s
    ScenarioSpecific "placeRival" _ -> do
      let Meta {rival} = toResult attrs.meta
      lead <- getLead
      rivalCard <- getSetAsideCard (factionStoryRival rival)
      push $ ReadStoryWithPlacement lead rivalCard ResolveIt Nothing Global
      pure s
    ScenarioSpecific "readInterlude" _ -> scope "theFabledJewel" do
      let Meta {ally} = toResult attrs.meta
      flavor do
        h "title"
        p "body"
        ul $ li (factionLabel ally)

      lead <- getLead
      alliedCard <- getSetAsideCard (factionStoryAllied ally)
      push $ ReadStoryWithPlacement lead alliedCard ResolveIt Nothing Global
      case ally of
        TheFoundation -> do
          lanternChamber <- selectJust $ locationIs Locations.lanternChamber
          createSetAsideEnemyWith_ Enemies.declanPearce lanternChamber createExhausted
          selectOne (mapOneOf enemyIs [Enemies.theBloodlessMan, Enemies.theBloodlessManUnleashed]) >>= \case
            Just theBloodlessMan -> createAssetAt_ Assets.jewelOfSarnath (AttachedToEnemy theBloodlessMan)
            Nothing -> createAssetAt_ Assets.jewelOfSarnath (AttachedToLocation lanternChamber)
        MiskatonicUniversity -> do
          rightmostSecondFloor <- selectJust $ LocationWithLabel "secondFloor3"
          declan <- createSetAsideEnemy Enemies.declanPearce rightmostSecondFloor
          createAssetAt_ Assets.jewelOfSarnath (AttachedToEnemy declan)
        TheSyndicate -> do
          top <- take 1 . unDeck <$> getEncounterDeck
          for_ top obtainCard
          jewel <- getSetAsideCard Assets.jewelOfSarnath
          declan <- getSetAsideCard Enemies.declanPearce
          cards <- shuffle $ jewel : declan : map toCard top
          secondFloor <- select $ LocationWithTrait SecondFloor
          zipWithM_ (\a b -> placeUnderneath a (only b)) secondFloor cards
        TheSilverTwilightLodge -> do
          shuffleEncounterDiscardBackIn
          doStep 1 msg
        LocalsOfKingsport -> do
          lanternChamber <- selectJust $ locationIs Locations.lanternChamber
          declan <- createSetAsideEnemyWith Enemies.declanPearce lanternChamber createExhausted
          jewelOfSarnath <- createAssetAt Assets.jewelOfSarnath (AttachedToEnemy declan)
          placeTokens ScenarioSource jewelOfSarnath #damage =<< perPlayer 1
          placeTokens ScenarioSource jewelOfSarnath #doom 1
      pure s
    DoStep 1 (ScenarioSpecific "readInterlude" _) -> scope "theFabledJewel" do
      top <- take 7 . unDeck <$> getEncounterDeck
      for_ top obtainCard
      jewel <- getSetAsideCard Assets.jewelOfSarnath
      declan <- getSetAsideCard Enemies.declanPearce
      cards <- shuffle $ jewel : declan : map toCard top
      lead <- getLead
      for_ cards $ putCardOnBottomOfDeck lead Deck.EncounterDeck
      n <- selectCount UneliminatedInvestigator
      if
        | n == 2 -> discardTopOfEncounterDeck lead ScenarioSource 6
        | n == 1 -> discardTopOfEncounterDeck lead ScenarioSource 12
        | otherwise -> pure ()
      pure s
    _ -> TheMidwinterGala <$> liftRunMessage msg attrs
