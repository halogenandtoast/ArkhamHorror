module Arkham.Scenario.Scenarios.WithoutATrace (withoutATrace) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheScarletKeys.Concealed
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Campaigns.TheScarletKeys.Key
import Arkham.Campaigns.TheScarletKeys.Meta
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Field
import Arkham.ForMovement
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Location (getCanMoveTo, withLocationOf)
import Arkham.Helpers.Log
import Arkham.Helpers.Modifiers (ModifierType (..), getModifiers)
import Arkham.Helpers.Query (allInvestigators, getLead, getPlayerCount)
import Arkham.Helpers.Xp
import Arkham.I18n
import Arkham.Id
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Message.Lifted.Move
import Arkham.Placement
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.WithoutATrace.Helpers
import Arkham.Trait (Trait (Outsider))
import Arkham.Window qualified as Window
import Data.Map.Strict qualified as Map

newtype WithoutATrace = WithoutATrace ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

withoutATrace :: Difficulty -> WithoutATrace
withoutATrace difficulty =
  scenario
    WithoutATrace
    "09681"
    "Without a Trace"
    difficulty
    []

instance HasChaosTokenValue WithoutATrace where
  getChaosTokenValue iid tokenFace (WithoutATrace attrs) = case tokenFace of
    Skull -> do
      x <- selectCount Anywhere
      pure $ toChaosTokenValue attrs Skull (x `div` 2) x
    Cultist -> pure $ toChaosTokenValue attrs Cultist 4 6
    Tablet -> do
      mods <- getModifiers iid
      let n = length [() | Hollow _ <- mods]
      pure $ toChaosTokenValue attrs Tablet (min n 5) n
    ElderThing -> pure $ ChaosTokenValue ElderThing (NegativeModifier 3)
    otherFace -> getChaosTokenValue iid otherFace attrs

handleCityOfRemnants
  :: ReverseQueue m
  => ScenarioAttrs
  -> Value
  -> (LocationsInShadows -> Maybe LocationId)
  -> (LocationsInShadows -> Maybe LocationId -> LocationsInShadows)
  -> m WithoutATrace
handleCityOfRemnants attrs v getLocation setLocation = do
  let (iid, c) :: (InvestigatorId, ConcealedCard) = toResult v
  let meta = toResult @LocationsInShadowsMetadata attrs.meta
  let locationsInShadows = meta.locationsInShadows
  runMaybeT_ do
    pos <- hoistMaybe $ case c.placement of
      InPosition x -> Just x
      _ -> Nothing
    lid <- hoistMaybe $ getLocation locationsInShadows
    lift do
      push $ PlaceGrid (GridLocation pos lid)
      newDecoy <- mkConcealedCard Decoy
      push $ Msg.CreateConcealedCard newDecoy
      newCards <- shuffle [c, newDecoy]
      grid <- getGrid
      case emptyPositionsInDirections grid pos [minBound ..] of
        [] -> pure ()
        [pos'] -> for_ newCards \newCard -> do
          push $ Msg.PlaceConcealedCard iid (toId newCard) (InPosition pos')
        [x, y] -> for_ (zip newCards [x, y]) \(newCard, pos') -> do
          push $ Msg.PlaceConcealedCard iid (toId newCard) (InPosition pos')
        ps -> case newCards of
          [a, b] -> chooseOneM iid do
            for_ (eachWithRest ps) \(pos', rest) -> do
              gridLabeled (gridLabel pos') do
                push $ Msg.PlaceConcealedCard iid (toId a) (InPosition pos')
                chooseOneM iid do
                  for_ rest \pos'' -> do
                    gridLabeled (gridLabel pos'') do
                      push $ Msg.PlaceConcealedCard iid (toId b) (InPosition pos'')
          _ -> error "expected exactly two cards"

  let otherworldDeck = fromJustNote "must be set" $ lookup OtherworldDeck attrs.decks
  let concealedCards = Map.map (filter (/= c.id)) meta.concealedCards
  case otherworldDeck of
    [] ->
      pure
        $ WithoutATrace
        $ attrs
        & (metaL .~ toJSON (meta {concealedCards, locationsInShadows = setLocation locationsInShadows Nothing}))
    (x : xs) -> do
      l <- placeLocation x
      push $ UpdateLocation l (Update LocationPlacement (Just InTheShadows))
      pure
        $ WithoutATrace
        $ attrs
        & (decksL . at OtherworldDeck ?~ xs)
        & ( metaL
              .~ toJSON (meta {concealedCards, locationsInShadows = setLocation locationsInShadows (Just l)})
          )

instance RunMessage WithoutATrace where
  runMessage msg s@(WithoutATrace attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      storyWithChooseOneM' (setTitle "title" >> p "intro1") do
        labeled' "blowTheWhistle" $ doStep 2 PreScenarioSetup
        labeled' "discardTheWhistle" $ doStep 3 PreScenarioSetup
      setupKeys
      pure s
    DoStep 2 PreScenarioSetup -> scope "intro" do
      record TheCellBlewTheWhistle
      flavor $ setTitle "title" >> p "intro2"
      doStep 4 PreScenarioSetup
      pure s
    DoStep 3 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "intro3"
      doStep 4 PreScenarioSetup
      pure s
    DoStep 4 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "intro4"
      pure s
    Setup -> runScenarioSetup WithoutATrace attrs do
      setup $ ul do
        li "gatherSets"
        li.nested "placeLocations" do
          li "startAt"
        li "blewTheWhistle"
        li "setAside"
        li.nested "otherworldDeck" do
          li "bottomOfDeck"
          li "topOfDeck"
          li "placeDeck"
        li "placeTopOfOtherworld"
        li "miniCards"
        li "placeMiniCards"
        unscoped $ li "shuffleRemainder"
        unscoped $ li "readyToBegin"

      setUsesGrid
      gather Set.WithoutATrace
      gather Set.AgentsOfTheOutside
      gather Set.BeyondTheBeyond
      gather Set.Outsiders
      gather Set.SecretWar
      gather Set.SpreadingCorruption
      handleRedCoterie

      setAgendaDeck [Agendas.otherworldlyHorror, Agendas.otherworldlyLambs, Agendas.otherworldlySlaughter]
      setActDeck [Acts.traversingTheOutside, Acts.redRuin, Acts.escapingTheOtherworld]

      courtOfTheOutsiders <- placeInGrid (Pos 0 0) Locations.courtOfTheOutsiders
      startAt courtOfTheOutsiders

      whenHasRecord TheCellBlewTheWhistle do
        alikiZoniUperetria <- createAsset =<< fetchCard Assets.alikiZoniUperetriaTheMaidWithTheScarletSash
        investigators <- allInvestigators
        leadChooseOneM do
          unscoped
            $ nameVar Assets.alikiZoniUperetriaTheMaidWithTheScarletSash
            $ questionLabeled' "chooseInvestigatorToTakeControlOf"
          questionLabeledCard Assets.alikiZoniUperetriaTheMaidWithTheScarletSash
          portraits investigators (`takeControlOfAsset` alikiZoniUperetria)

      removeEvery [Assets.alikiZoniUperetriaTheMaidWithTheScarletSash]

      setAside
        [ Assets.theRedGlovedManHeWasAlwaysThere
        , Enemies.mimeticNemesisOtherworldlySubjugator
        , Enemies.protoplasmicReassembler
        , Enemies.apocalypticPresage
        ]

      n <-
        getPlayerCount <&> \case
          1 -> 2
          2 -> 1
          _ -> 0
      cliffs <- fromGathered (cardIs Locations.cliffsOfInsanity)
      outsidersLair <- fromGathered (cardIs Locations.outsidersLairWithoutATrace)
      bottom <- shuffle $ outsidersLair <> cliffs
      (inPlay, top) <-
        fmap (splitAt 3 . drop n) . shuffle =<< fromGathered (CardWithTitle "City of Remnants")

      addExtraDeck OtherworldDeck $ top <> bottom

      case inPlay of
        [x, y, z] -> do
          ll <- placeLocation x
          ml <- placeLocation y
          rl <- placeLocation z
          for_ [ll, ml, rl] \location -> do
            push $ UpdateLocation location (Update LocationPlacement (Just InTheShadows))
          setMeta
            $ LocationsInShadowsMetadata
              { locationsInShadows = LocationsInShadows (Just ll) (Just ml) (Just rl)
              , concealedCards = mempty
              }
        _ -> error "expected exactly three locations in play"

      cards <-
        shuffle =<< traverse mkConcealedCard [CityOfRemnantsL, CityOfRemnantsM, CityOfRemnantsR, Decoy]

      lead <- getLead
      for_ (zip cards [Pos 1 0, Pos 0 1, Pos (-1) 0, Pos 0 (-1)]) \(card, pos) -> do
        push $ Msg.CreateConcealedCard card
        push $ Msg.PlaceConcealedCard lead (toId card) (InPosition pos)
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
    ForTarget (LocationTarget loc) (ScenarioSpecific x v) | x `elem` ["exposed[CityOfRemnantsL]", "exposed[CityOfRemnantsM]", "exposed[CityOfRemnantsR]"] -> do
      let (iid, _c) :: (InvestigatorId, ConcealedCard) = toResult v
      withLocationOf iid \current -> do
        whenMatch loc (ConnectedTo ForMovement $ LocationWithId current) do
          whenM (getCanMoveTo iid attrs loc) do
            checkAfter $ Window.ScenarioEvent "exposedAdjacentLocation" (Just iid) (toJSON loc)

      checkAfter $ Window.CampaignEvent "exposed[location]" (Just iid) Null
      pure s
    Do (ScenarioSpecific "exposed[CityOfRemnantsL]" v) -> do
      handleCityOfRemnants attrs v (.left) \locations ml -> locations {left = ml}
    Do (ScenarioSpecific "exposed[CityOfRemnantsM]" v) -> do
      handleCityOfRemnants attrs v (.middle) \locations ml -> locations {middle = ml}
    Do (ScenarioSpecific "exposed[CityOfRemnantsR]" v) -> do
      handleCityOfRemnants attrs v (.right) \locations ml -> locations {right = ml}
    PlaceConcealedCard _ card (InPosition pos) -> do
      let meta = toResult @LocationsInShadowsMetadata attrs.meta
      let current = Map.findWithDefault [] pos meta.concealedCards
      cards <- shuffleM $ nub $ card : current
      let concealedCards = Map.map (filter (/= card)) meta.concealedCards
      pure
        $ WithoutATrace
        $ attrs
        & metaL
        .~ toJSON (meta {concealedCards = Map.insert pos cards concealedCards})
    Do (PlaceConcealedCard _ card (InPosition pos)) -> do
      let meta = toResult @LocationsInShadowsMetadata attrs.meta
      let current = Map.findWithDefault [] pos meta.concealedCards
      cards <- shuffleM $ nub $ card : current
      let concealedCards = Map.map (filter (/= card)) meta.concealedCards
      pure
        $ WithoutATrace
        $ attrs
        & metaL
        .~ toJSON (meta {concealedCards = Map.insert pos cards concealedCards})
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        Cultist -> do
          outsiders <- select $ NearestEnemyTo iid $ EnemyWithTrait Outsider
          withLocationOf iid \lid -> do
            chooseOrRunOneM iid do
              targets outsiders \outsider -> do
                readyThis outsider
                moveTowards Cultist outsider lid
                ifEnemy outsider (enemyAtLocationWith iid) do
                  initiateEnemyAttack outsider Cultist iid
        ElderThing -> do
          cards <- select $ inHandOf NotForPlay iid <> basic NonWeakness
          chooseTargetM iid cards $ hollow iid
        _ -> pure ()
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
        $ WithoutATrace
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
        $ WithoutATrace
        $ attrs
        & (metaL .~ toJSON (meta {concealedCards}))
    ScenarioSpecific "distributeConcealedLocations" v -> do
      let (iid, cs, current, original) :: (InvestigatorId, [ConcealedCard], [Pos], [Pos]) = toResult v
      case cs of
        [] -> pure ()
        (x : xs) ->
          if null current
            then scenarioSpecific "distributeConcealedLocations" (iid, cs, original, original)
            else chooseOneM iid do
              for_ (eachWithRest current) \(pos, rest) -> do
                gridLabeled (gridLabel pos) do
                  push $ PlaceConcealedCard iid (toId x) (InPosition pos)
                  scenarioSpecific "distributeConcealedLocations" (iid, xs, rest, original)
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> do
          record TheCellWasHollowed
          resolution "noResolution"
          gameOver
        Resolution 1 -> do
          record TheCellKnowsTheTrueNatureOfTheCoterie
          record AlikiIsOnYourSide
          resolutionWithXp "resolution1" $ allGainXpWithBonus' attrs $ toBonus "bonus" 1
          markTime 3
          leadChooseOneM do
            labeled' "bermuda" $ campaignSpecific "setCurrent" Bermuda
            labeled' "yborCity" $ campaignSpecific "setCurrent" YborCity
            labeled' "sanJuan" $ campaignSpecific "setCurrent" SanJuan
          endOfScenario
        Resolution 2 -> do
          record TheCellKnowsTheTrueNatureOfTheCoterie
          record YouHaventSeenTheLastOfAlikiZoniUperetria
          resolutionWithXp "resolution2" $ allGainXpWithBonus' attrs $ toBonus "bonus" 1
          markTime 2
          leadChooseOneM do
            labeled' "bermuda" $ campaignSpecific "setCurrent" Bermuda
            labeled' "yborCity" $ campaignSpecific "setCurrent" YborCity
            labeled' "sanJuan" $ campaignSpecific "setCurrent" SanJuan
          endOfScenario
        Resolution 3 -> do
          record AlikiIsOnYourSide
          resolutionWithXp "resolution3" $ allGainXp' attrs
          markTime 2
          leadChooseOneM do
            labeled' "bermuda" $ campaignSpecific "setCurrent" Bermuda
            labeled' "yborCity" $ campaignSpecific "setCurrent" YborCity
            labeled' "sanJuan" $ campaignSpecific "setCurrent" SanJuan
          endOfScenario
        Resolution 4 -> do
          record YouHaventSeenTheLastOfAlikiZoniUperetria
          resolutionWithXp "resolution4" $ allGainXp' attrs
          leadChooseOneM do
            labeled' "bermuda" $ campaignSpecific "setCurrent" Bermuda
            labeled' "yborCity" $ campaignSpecific "setCurrent" YborCity
            labeled' "sanJuan" $ campaignSpecific "setCurrent" SanJuan
          markTime 1
          endOfScenario
        _ -> error "Unknown resolution for Dead Heat"
      pure s
    _ -> WithoutATrace <$> liftRunMessage msg attrs
