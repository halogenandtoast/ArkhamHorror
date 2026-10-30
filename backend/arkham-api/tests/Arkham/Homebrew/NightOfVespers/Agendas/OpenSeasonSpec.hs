module Arkham.Homebrew.NightOfVespers.Agendas.OpenSeasonSpec (spec) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Entities qualified as Entities
import Arkham.Helpers.Location (getCanStepTo)
import Arkham.Homebrew.NightOfVespers.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.NightOfVespers.CardDefs.Enemies qualified as Enemies
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Location.Base (connectedMatchersL, revealedConnectedMatchersL, revealedL)
import Arkham.Matcher
import Arkham.Treachery.CardDefs.NightOfTheZealot qualified as Treacheries
import TestImport.New

installAgenda :: TestAppT ()
installAgenda = do
  card <- genCard Agendas.openSeason
  let agenda = lookupAgenda (AgendaId $ toCardCode card) 1 (toCardId card)
  overTest $ entitiesL . Entities.agendasL %~ insertEntity agenda
  env <- get
  runReaderT (overGameM preloadModifiers) env

-- A -> B <- C. Reversing B -> A/B -> C changes distances from A without
-- changing any printed connections or movement permissions.
board :: TestAppT (Location, Location, Location)
board = do
  b <- testLocationWith $ revealedL .~ True
  let pointsToB = (connectedMatchersL .~ [be b.id]) . (revealedConnectedMatchersL .~ [be b.id])
  a <- testLocationWith $ pointsToB . (revealedL .~ True)
  c <- testLocationWith $ pointsToB . (revealedL .~ True)
  pure (a, b, c)

spec :: Spec
spec = describe "Open Season" do
  it "uses two-way distances for encounter cards without changing connections" . gameTest $ \self -> do
    (a, b, c) <- board
    self `moveTo` a
    installAgenda
    card <- genCard Enemies.skulkingHooligan
    overTest $ resolvingCardL ?~ card
    select (FarthestLocationFromAll RevealedLocation) `shouldReturn` [c.id]
    d <-
      testLocationWith $ (connectedMatchersL .~ [be c.id]) . (revealedConnectedMatchersL .~ [be c.id])
    select (NearestLocationToLocation b.id $ oneOf [be a.id, be d.id]) `shouldReturn` [a.id]
    select (connectedFrom $ be b.id) `shouldReturn` []

  it "also applies to a weakness treachery drawn from a player deck" . gameTest $ \self -> do
    (a, _, c) <- board
    self `moveTo` a
    installAgenda
    card <- genCard Treacheries.paranoia
    overTest $ resolvingCardL ?~ card
    select (FarthestLocationFromInvestigator (be self.id) RevealedLocation) `shouldReturn` [c.id]

  it "also applies while resolving an in-play treachery ability" . gameTest $ \self -> do
    (a, _, c) <- board
    self `moveTo` a
    installAgenda
    card <- genCard Treacheries.paranoia
    overTest
      $ (resolvingCardL .~ Nothing)
      . (activeAbilitiesL .~ [mkAbility (SourceableWithCardCode card card.id) 1 $ forced AnyWindow])
    select (FarthestLocationFromAll RevealedLocation) `shouldReturn` [c.id]

  it "keeps directed distances for player cards" . gameTest $ \self -> do
    (a, b, _) <- board
    self `moveTo` a
    installAgenda
    card <- genCard Assets.flashlight
    overTest $ resolvingCardL ?~ card
    select (FarthestLocationFromAll RevealedLocation) `shouldReturn` [b.id]

  it "keeps directed distances without Open Season" . gameTest $ \self -> do
    (a, b, _) <- board
    self `moveTo` a
    card <- genCard Enemies.skulkingHooligan
    overTest $ resolvingCardL ?~ card
    select (FarthestLocationFromAll RevealedLocation) `shouldReturn` [b.id]

  it "spawns Skulking Hooligan using the modified furthest-location query" . gameTest $ \self -> do
    (a, _, c) <- board
    self `moveTo` a
    installAgenda
    hooligan <- testEnemyWithDef Enemies.skulkingHooligan id
    overTest $ resolvingCardL ?~ toCard hooligan
    run $ InvestigatorDrawEnemy self.id hooligan.id
    assertAny $ EnemyWithId hooligan.id <> EnemyAt (be c.id)

  it "returns half the investigator's clues, rounded up, when Hooligan engages" . gameTest $ \self -> do
    location <- testLocationWith $ revealedL .~ True
    self `moveTo` location
    withProp @"clues" 3 self
    hooligan <- testEnemyWithDef Enemies.skulkingHooligan id
    hooligan `spawnAt` location
    useForcedAbility
    clickLabel "$nightOfVespers.theEndOfSeasons.label.skulkingHooligan.returnClues"
    self.clues `shouldReturn` 1

  it "allows the group to pay clues from other locations" . gameTest $ \self -> do
    (a, _, _) <- board
    elsewhere <- testLocation
    other <- addInvestigator Investigators.rolandBanks
    self `moveTo` a
    other `moveTo` elsewhere
    destination <- testLocation
    installAgenda
    getModifiers destination
      `shouldReturn` [AdditionalCostToEnter $ GroupClueCost (PerPlayer 1) Anywhere]
    getModifiers a `shouldReturn` []
    withProp @"clues" 1 other
    getCanStepTo self.id (TestSource mempty) destination.id `shouldReturn` False
    withProp @"clues" 2 other
    getCanStepTo self.id (TestSource mempty) destination.id `shouldReturn` True
