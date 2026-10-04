{- | Consternation on the Constellation -- a fan side story by the hosts of the
Mythos Busters podcast, made for Gen Con 2019.

The SS Constellation is three decks deep and sinking. Every location prints its
own symbol and connections, so the grid below is layout only; what orders the
ship is the @Deck@ trait, because the water rises from the bottom and the cards
that flood a location always take the lowest ready @Deck@ number first.

The scenario forks once, on whichever deck advances first:

* __Act 2 first__ -- the investigators find the Tablet of Dagon. Act 3a is "Flee
  the Ship", agenda 3a is "Punish the Interlopers", and the Colossal Servant
  spawns at Open Water.
* __Agenda 2 first__ -- the cult finds it. Act 3a is "Plug the Abyss", agenda 3a
  is "Summon Those Below", and Luther Marsh spawns at the Bridge holding the
  tablet.

Only one side of each pair is ever used, so both act 3s and both agenda 3s stay
set aside until the branch is taken.
-}
module Arkham.Homebrew.ConsternationOnTheConstellation.Scenarios.ConsternationOnTheConstellation (
  consternationOnTheConstellation,
) where

import Arkham.Card.CardDef (CardDef)
import Arkham.Difficulty
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Treacheries qualified as Treacheries
import Arkham.Homebrew.ConsternationOnTheConstellation.Helpers
import Arkham.Homebrew.ConsternationOnTheConstellation.Sets qualified as Set
import Arkham.Matcher
import Arkham.Scenario.Import.Lifted

newtype ConsternationOnTheConstellation = ConsternationOnTheConstellation ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

consternationOnTheConstellation :: Difficulty -> ConsternationOnTheConstellation
consternationOnTheConstellation difficulty =
  sideStory
    ConsternationOnTheConstellation
    ":consternation-on-the-constellation:001"
    "Consternation on the Constellation"
    difficulty
    [ "deckLoungeAndTheatre sunDeck          bridge   ."
    , "diningRoom           passengerCabins  library  ."
    , "galley               cargoRoom        .        lifeboat"
    , "engineRoom           boilerRoom       .        openWater"
    ]

instance HasChaosTokenValue ConsternationOnTheConstellation where
  getChaosTokenValue iid chaosTokenFace (ConsternationOnTheConstellation attrs) = case chaosTokenFace of
    -- "-2 (-4 instead if your location is exhausted)" on Easy/Standard, -3/-5 on
    -- Hard/Expert. The exhausted half is a modifier the token applies on reveal.
    Skull -> pure $ toChaosTokenValue attrs Skull 2 3
    Cultist -> pure $ toChaosTokenValue attrs Cultist 2 3
    Tablet -> pure $ toChaosTokenValue attrs Tablet 3 4
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 4 5
    otherFace -> getChaosTokenValue iid otherFace attrs

{- | The five crates. Setup keeps the Tablet of Dagon and two of the other four,
so which crate holds what is hidden.
-}
crates :: [CardDef]
crates =
  [ Assets.crateOfGoodsCrimsonLedger
  , Assets.crateOfGoodsHandOfTheStrangler
  , Assets.crateOfGoodsAbyssalSword
  , Assets.crateOfGoodsRingOfTheDeep
  ]

-- | Everything act 1b (or agenda 1b) puts into play at once. Lifeboat is not here.
additionalLocations :: [CardDef]
additionalLocations =
  [ Locations.engineRoom
  , Locations.boilerRoom
  , Locations.galley
  , Locations.diningRoom
  , Locations.passengerCabins
  , Locations.library
  , Locations.deckLoungeAndTheatre
  , Locations.sunDeck
  , Locations.bridge
  ]

instance RunMessage ConsternationOnTheConstellation where
  runMessage msg (ConsternationOnTheConstellation attrs) = runQueueT $ scenarioI18n $ case msg of
    Setup -> runScenarioSetup ConsternationOnTheConstellation attrs do
      setup $ ul do
        li "gatherSets"
        li "placeLocations"
        li "takingOnWater"
        li "crateOfGoods"
        li "setAside"
        li "orderEnforcer"
        unscoped $ li "shuffleRemainder"

      gather Set.ConsternationOnTheConstellation
      gather Set.DeepOnes
      gather Set.SinkingShip

      -- "Put Cargo Room and Open Water into play. Each investigator begins play
      -- at the Cargo Room. Set each other location aside, out of play."
      cargoRoom <- placeLabeled "cargoRoom" Locations.cargoRoom
      void $ placeLabeled "openWater" Locations.openWater
      startAt cargoRoom

      {- "Remove 1 copy of Taking on Water from the game for each player beyond
      the first." Removed before the Sinking Ship set is put aside, because only
      the gathered deck can be thinned a copy at a time. -}
      playerCount <- getPlayerCount
      removeOneOfEach $ replicate (playerCount - 1) Treacheries.takingOnWater

      {- "Shuffle the Tablet of Dagon and 2 other random Crate of Goods assets
      together. Remove the remaining Crate of Goods assets from the game." The
      three survivors stay on their unrevealed Crate of Goods side. -}
      (keptCrates, removedCrates) <- splitAt 2 <$> shuffleM crates
      removeEvery removedCrates

      {- "Search the gathered encounter sets for 1 copy of Order Enforcer. Spawn
      it at Cargo Room." The other copy stays in the encounter deck. -}
      orderEnforcer <- fromGathered1 Enemies.orderEnforcer
      createEnemyAt_ orderEnforcer cargoRoom

      setAside
        $ Assets.crateOfGoodsTabletOfDagon
        : keptCrates
          <> additionalLocations
          <> [ Locations.lifeboat
             , Enemies.lutherMarsh
             , Assets.inspectorLegrasse
             ]

      -- "Set ... each card from the Deep Ones and Sinking Ship encounter sets
      -- aside, out of play."
      setAsideEvery $ CardFromEncounterSet Set.DeepOnes
      setAsideEvery $ CardFromEncounterSet Set.SinkingShip

      {- Only agenda 1-2 and act 1-2 are dealt. Whichever deck advances first
      names its own stage 3 (@Acts.fleeTheShip@ / @Agendas.punishTheInterlopers@,
      or @Acts.plugTheAbyss@ / @Agendas.summonThoseBelow@), so the other half of
      each pair is never used. -}
      setAgendaDeck [Agendas.captured, Agendas.searchTheShip]
      setActDeck [Acts.thinkFast, Acts.findItFirst]
    _ -> ConsternationOnTheConstellation <$> liftRunMessage msg attrs
