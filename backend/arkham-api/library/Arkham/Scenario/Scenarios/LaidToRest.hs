module Arkham.Scenario.Scenarios.LaidToRest (laidToRest, LaidToRest (..)) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Campaign (getCampaignStoryCards, matchingCardsAlreadyInDeck)
import Arkham.Helpers.Card (ConvertToCard (..), getVictoryPoints)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers hiding (roundModifiers)
import Arkham.I18n
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Name (toTitle)
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Types (Field (..))
import Arkham.Scenarios.LaidToRest.Helpers
import Arkham.Tracing
import Arkham.Trait (Trait (Geist, Spectral))
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Xp

newtype LaidToRest = LaidToRest ScenarioAttrs
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor LaidToRest where
  getModifiersFor (LaidToRest a) = do
    modifySelect
      a
      (InvestigatorAt $ LocationWithTrait Spectral)
      [UseEncounterDeck SpectralEncounterDeck]
    spectral <- findAllCards (`cardMatch` CardWithTrait Spectral)
    modifyEach a (map (CardIdTarget . toCardId) spectral) [UseEncounterDeck SpectralEncounterDeck]

laidToRest :: Difficulty -> LaidToRest
laidToRest difficulty =
  sideStory
    LaidToRest
    "90054"
    "Laid to Rest"
    difficulty
    [ ".              theGallows    .             chapelAttic     ."
    , "hereticsGraves hauntedFields .             abandonedChapel chapelCrypt"
    , ".              .             hangmansBrook .               ."
    ]

instance HasChaosTokenValue LaidToRest where
  getChaosTokenValue iid chaosTokenFace (LaidToRest attrs) = case chaosTokenFace of
    Skull -> do
      n <- selectCount $ VictoryDisplayCardMatch $ basic $ CardWithTitle "Unfinished Business"
      pure $ toChaosTokenValue attrs Skull (n + 1) (n + 2)
    Cultist -> do
      n <- cardsAttachedToTheBeyond
      pure $ toChaosTokenValue attrs Cultist n (n * 2)
    Tablet -> do
      n <- selectCount $ LocationWithTrait Spectral
      pure $ toChaosTokenValue attrs Tablet n (n * 2)
    ElderThing ->
      if isEasyStandard attrs
        then pure $ ChaosTokenValue ElderThing (NegativeModifier 2)
        else do
          geist <- selectAny $ EnemyWithTrait Geist <> enemyAtLocationWith iid
          pure $ ChaosTokenValue ElderThing $ if geist then AutoFailModifier else NegativeModifier 3
    otherFace -> getChaosTokenValue iid otherFace attrs

{- FOURMOLU_DISABLE -}
easyTokens, standardTokens, hardTokens, expertTokens :: [ChaosTokenFace]
easyTokens =
  [ PlusOne , PlusOne , Zero , Zero , Zero , MinusOne , MinusOne , MinusOne , MinusTwo , MinusTwo
  , Skull , Skull , Skull , Cultist , Tablet , ElderThing , AutoFail , ElderSign
  ]
standardTokens =
  [ PlusOne , Zero , Zero , MinusOne , MinusOne , MinusOne , MinusTwo , MinusTwo , MinusThree
  , MinusFour , Skull , Skull , Skull , Cultist , Tablet , ElderThing , AutoFail , ElderSign
  ]
hardTokens =
  [ Zero , Zero , MinusOne , MinusOne , MinusTwo , MinusTwo , MinusThree , MinusFour , MinusFive
  , MinusSix , Skull , Skull , Skull , Cultist , Tablet , ElderThing , AutoFail , ElderSign
  ]
expertTokens =
  [ Zero , MinusOne , MinusTwo , MinusThree , MinusFour , MinusFive , MinusSix , MinusSeven
  , MinusEight , Skull , Skull , Skull , Cultist , Tablet , ElderThing , AutoFail , ElderSign
  ]
{- FOURMOLU_ENABLE -}

hasCampaignCard
  :: (HasGame m, Tracing m, HasCardDef def) => InvestigatorId -> def -> m Bool
hasCampaignCard iid (toCardDef -> def) = do
  inDeck <- matchingCardsAlreadyInDeck (cardIs def)
  storyCards <- getCampaignStoryCards
  pure
    $ maybe False notNull (lookup iid inDeck)
    || any ((== def) . toCardDef) (findWithDefault [] iid storyCards)

gainLaidToRestXp :: forall m. ReverseQueue m => ScenarioAttrs -> m (Int, Int)
gainLaidToRestXp attrs = do
  victoryCards <- scenarioField ScenarioVictoryDisplay
  let enemyCards = filter ((`elem` [EnemyType, PlayerEnemyType]) . cdCardType . toCardDef) victoryCards
  let locationCards = filter ((== LocationType) . cdCardType . toCardDef) victoryCards
  cluelessLocations <- select $ RevealedLocation <> LocationWithoutClues
  enemyEntries <- toVictoryEntries enemyCards
  locationEntries <- (<>) <$> toVictoryEntries locationCards <*> toVictoryEntries cluelessLocations
  jims <- select $ IncludeEliminated $ jimCulver <> InvestigatorCanGainXp
  others <- select $ IncludeEliminated $ not_ jimCulver <> InvestigatorCanGainXp
  let jimXp = sum $ map snd enemyEntries
  let otherXp = sum $ map snd locationEntries
  push
    $ ReportXp
    $ XpBreakdown
    $ [ InvestigatorGainXp jim $ XpDetail XpFromVictoryDisplay title n
      | jim <- jims
      , (title, n) <- enemyEntries
      ]
    <> [ InvestigatorGainXp other $ XpDetail XpFromVictoryDisplay title n
       | other <- others
       , (title, n) <- locationEntries
       ]
  for_ jims \jim -> push $ GainXP jim (toSource attrs) jimXp
  for_ others \other -> push $ GainXP other (toSource attrs) otherXp
  pure (jimXp, otherXp)
 where
  toVictoryEntries :: (ConvertToCard c, HasGame m, Tracing m) => [c] -> m [(Text, Int)]
  toVictoryEntries = mapMaybeM \c -> do
    card <- convertToCard c
    mvp <- getVictoryPoints c
    pure $ fmap (\n -> (toTitle card, n)) mvp

instance RunMessage LaidToRest where
  runMessage msg s@(LaidToRest attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "body"
      pure s
    StandaloneSetup -> do
      setChaosTokens $ case attrs.difficulty of
        Easy -> easyTokens
        Standard -> standardTokens
        Hard -> hardTokens
        Expert -> expertTokens
      pure s
    Setup -> runScenarioSetup LaidToRest attrs do
      setup $ ul do
        li "spiritDeck"
        li "gatherSets"
        li "removeCards"
        li.nested "placeLocations" do
          li "nonSpectral"
          li "clues"
          li "startAt"
        li "heretics"
        li "jeanDevereux"
        li "ravenousSpirits"
        li.nested "encounterDecks" do
          li "spectralDeck"
          li "standardDeck"
        unscoped $ li "readyToBegin"

      scope "theSpectralEncounterDeck" $ flavor $ h "title" >> p "body"
      scope "theBeyondAndTheSpiritDeck" $ flavor $ h "title" >> p "body"

      gather Set.LaidToRest
      gather Set.TheWagesOfSin
      gather Set.AnettesCoven
      gather Set.CityOfSins
      gather Set.InexorableFate
      gather Set.RealmOfDeath
      gather Set.TrappedSpirits
      gather Set.Witchcraft

      removeEvery [Assets.spectralWeb]

      setAgendaDeck [Agendas.gatheringMists]
      setActDeck [Acts.laidToRest]

      hangmansBrook <- place Locations.hangmansBrook
      startAt hangmansBrook

      theGallows <- placeOneOf (Locations.theGallows_169, Locations.theGallows_170)
      hereticsGraves <- placeOneOf (Locations.hereticsGraves_171, Locations.hereticsGraves_172)
      chapelCrypt <- placeOneOf (Locations.chapelCrypt_173, Locations.chapelCrypt_174)
      removeEvery [Locations.chapelAttic_175]
      chapelAttic <- place Locations.chapelAttic_176
      hauntedFields <- place Locations.hauntedFields
      abandonedChapel <- place Locations.abandonedChapel

      clues <- perPlayer 1
      for_ [hangmansBrook, theGallows, hereticsGraves, chapelCrypt, chapelAttic, hauntedFields, abandonedChapel] \lid ->
        placeTokens ScenarioSource lid #clue clues

      heretics <-
        pickN
          4
          [ Enemies.heretic_A
          , Enemies.heretic_C
          , Enemies.heretic_E
          , Enemies.heretic_G
          , Enemies.heretic_I
          , Enemies.heretic_K
          ]
      addToSpiritDeck =<< traverse genCard heretics

      enemyAt_ Enemies.jeanDevereuxSeekingClosure hangmansBrook

      spirits <- fromGathered (cardIs Enemies.ravenousSpirit)
      for_ (zip spirits [theGallows, hereticsGraves, chapelAttic, chapelCrypt]) \(spirit, lid) ->
        placeUnderneath lid [spirit]

      setExtraEncounterDeck SpectralEncounterDeck
        =<< amongGathered (SingleSidedCard <> CardWithTrait Spectral <> not_ #location)
    ResolveChaosToken _ ElderThing iid -> do
      when (isEasyStandard attrs) do
        geist <- selectAny $ EnemyWithTrait Geist <> enemyAtLocationWith iid
        when geist $ drawAnotherChaosToken iid
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> do
          resolution "noResolution"
          push R2
        Resolution 1 -> do
          (jimXp, otherXp) <- gainLaidToRestXp attrs
          withVars ["jimXp" .= jimXp, "xp" .= otherXp] $ resolution "resolution1"
          mJim <- selectOne $ IncludeEliminated jimCulver
          for_ mJim \jim -> do
            hasTrumpet <- hasCampaignCard jim Assets.jimsTrumpet
            hasAdvancedRhapsody <- hasCampaignCard jim Treacheries.finalRhapsodyAdvanced
            scenarioI18n $ scope "label" $ chooseOneM jim do
              when hasTrumpet $ labeled' "upgradeJimsTrumpet" do
                removeCampaignCardFromDeck jim Assets.jimsTrumpet
                addCampaignCardToDeck jim DoNotShuffleIn Assets.jimsTrumpetAdvanced
              when hasAdvancedRhapsody $ labeled' "downgradeFinalRhapsody" do
                removeCampaignCardFromDeck jim Treacheries.finalRhapsodyAdvanced
                addCampaignCardToDeck jim DoNotShuffleIn Treacheries.finalRhapsody
              labeled' "doNotSwap" nothing
          endOfScenario
        Resolution 2 -> do
          (jimXp, otherXp) <- gainLaidToRestXp attrs
          withVars ["jimXp" .= jimXp, "xp" .= otherXp] $ resolution "resolution2"
          mJim <- selectOne $ IncludeEliminated jimCulver
          for_ mJim \jim -> do
            hasRhapsody <- hasCampaignCard jim Treacheries.finalRhapsody
            hasAdvancedTrumpet <- hasCampaignCard jim Assets.jimsTrumpetAdvanced
            when (hasRhapsody || hasAdvancedTrumpet) do
              scenarioI18n $ scope "label" $ chooseOrRunOneM jim do
                when hasRhapsody $ labeled' "upgradeFinalRhapsody" do
                  removeCampaignCardFromDeck jim Treacheries.finalRhapsody
                  addCampaignCardToDeck jim DoNotShuffleIn Treacheries.finalRhapsodyAdvanced
                when hasAdvancedTrumpet $ labeled' "downgradeJimsTrumpet" do
                  removeCampaignCardFromDeck jim Assets.jimsTrumpetAdvanced
                  addCampaignCardToDeck jim DoNotShuffleIn Assets.jimsTrumpet
          endOfScenario
        _ -> error "invalid resolution"
      pure s
    _ -> LaidToRest <$> liftRunMessage msg attrs
