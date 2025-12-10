module Arkham.Scenario.Scenarios.WithoutATrace (withoutATrace) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheScarletKeys.Concealed
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Campaigns.TheScarletKeys.Key
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Log
import Arkham.Helpers.Query (allInvestigators, getLead, getPlayerCount)
import Arkham.I18n
import Arkham.Id
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid
import Arkham.Matcher.Card
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Placement
import Arkham.Scenario.Deck
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.WithoutATrace.Helpers
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
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

handleCityOfRemnants
  :: ReverseQueue m
  => InvestigatorId -> ConcealedCard -> LocationsInShadows -> (LocationsInShadows -> Maybe Card) -> m ()
handleCityOfRemnants iid c locationsInShadows getCard =
  runMaybeT_ do
    pos <- hoistMaybe $ case c.placement of
      InPosition x -> Just x
      _ -> Nothing
    card <- hoistMaybe $ getCard locationsInShadows
    lift do
      placeLocationInGrid_ pos card
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
      outsidersLair <- fetchCard Locations.outsidersLairWithoutATrace
      bottom <- shuffle $ outsidersLair : cliffs
      (inPlay, top) <-
        fmap (splitAt 3 . drop n) . shuffle =<< fromGathered (CardWithTitle "City of Remnants")

      addExtraDeck OtherworldDeck $ top <> bottom

      case inPlay of
        [x, y, z] ->
          setMeta
            $ LocationsInShadowsMetadata
              { locationsInShadows = LocationsInShadows (Just x) (Just y) (Just z)
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
      let (iid, c) :: (InvestigatorId, ConcealedCard) = toResult v
      let meta = toResult @LocationsInShadowsMetadata attrs.meta
      let locationsInShadows = meta.locationsInShadows
      handleCityOfRemnants iid c locationsInShadows (.left)

      let otherworldDeck = fromJustNote "must be set" $ lookup OtherworldDeck attrs.decks
      let concealedCards = Map.map (filter (/= c.id)) meta.concealedCards
      pure $ WithoutATrace $ case otherworldDeck of
        [] -> attrs
        (x : xs) ->
          attrs
            & (decksL . at OtherworldDeck ?~ xs)
            & (metaL .~ toJSON (meta {concealedCards, locationsInShadows = locationsInShadows {left = Just x}}))
    ScenarioSpecific "exposed[CityOfRemnantsM]" v -> do
      let (iid, c) :: (InvestigatorId, ConcealedCard) = toResult v
      let meta = toResult @LocationsInShadowsMetadata attrs.meta
      let locationsInShadows = meta.locationsInShadows
      handleCityOfRemnants iid c meta.locationsInShadows (.middle)

      let otherworldDeck = fromJustNote "must be set" $ lookup OtherworldDeck attrs.decks
      let concealedCards = Map.map (filter (/= c.id)) meta.concealedCards
      pure $ WithoutATrace $ case otherworldDeck of
        [] -> attrs
        (x : xs) ->
          attrs
            & (decksL . at OtherworldDeck ?~ xs)
            & (metaL .~ toJSON (meta {concealedCards, locationsInShadows = locationsInShadows {middle = Just x}}))
    ScenarioSpecific "exposed[CityOfRemnantsR]" v -> do
      let (iid, c) :: (InvestigatorId, ConcealedCard) = toResult v
      let meta = toResult @LocationsInShadowsMetadata attrs.meta
      let locationsInShadows = meta.locationsInShadows
      handleCityOfRemnants iid c locationsInShadows (.right)

      let otherworldDeck = fromJustNote "must be set" $ lookup OtherworldDeck attrs.decks
      let concealedCards = Map.map (filter (/= c.id)) meta.concealedCards
      pure $ WithoutATrace $ case otherworldDeck of
        [] -> attrs
        (x : xs) ->
          attrs
            & (decksL . at OtherworldDeck ?~ xs)
            & (metaL .~ toJSON (meta {concealedCards, locationsInShadows = locationsInShadows {right = Just x}}))
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
    _ -> WithoutATrace <$> liftRunMessage msg attrs
