module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Locations.TidalPool (tidalPool) where

import Arkham.Ability
import Arkham.Discover (DiscoverLocation (..))
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Helpers.Scenario
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Helpers (deepOneInvestigator)
import Arkham.Key
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Types

newtype TidalPool = TidalPool LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tidalPool :: LocationCard TidalPool
tidalPool = locationWith TidalPool Cards.tidalPool 3 (PerPlayer 2) connectsToAdjacent

{- | "A maximum of 1 per player clues can be discovered from this location per round."

The round's running total lives in the location's own meta. Reporting the *remaining*
allowance as 'MaxCluesDiscovered' lets the engine's existing per-discovery cap do the
clamping, which is why the tally has to be a round total rather than a per-test one.
-}
instance HasModifiersFor TidalPool where
  getModifiersFor (TidalPool a) = whenRevealed a do
    n <- getPlayerCount
    modifySelf a [MaxCluesDiscovered $ max 0 (n - discoveredThisRound a)]

discoveredThisRound :: LocationAttrs -> Int
discoveredThisRound a = toResultDefault 0 a.meta

instance HasAbilities TidalPool where
  getAbilities (TidalPool a) =
    extendRevealed a
      $ [restricted a 1 UnrevealedKeyIsSetAside $ forced $ RevealLocation #after Anyone (be a)]
      <> [ restricted
             a
             2
             (Here <> thisExists a FloodedLocation <> youExist deepOneInvestigator)
             (FastAbility Free)
         | notNull a.keys
         ]

instance RunMessage TidalPool where
  runMessage msg l@(TidalPool attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      -- "Randomly choose 1 of the set-aside facedown keys and place it on Tidal Pool
      -- without looking at it."
      let
        unrevealed = \case
          UnrevealedKey _ -> True
          _ -> False
      unrevealedKeys <- filter unrevealed . setToList <$> scenarioField ScenarioSetAsideKeys
      for_ (nonEmpty unrevealedKeys) $ sample >=> placeKey (toTarget attrs)
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      -- "Take control of a key on Tidal Pool."
      chooseOneM iid $ for_ (toList attrs.keys) \k -> keyLabeled k $ placeKey iid k
      pure l
    Msg.DiscoverClues _ d | d.location == DiscoverAtLocation attrs.id -> do
      l' <- liftRunMessage msg attrs
      pure $ TidalPool $ setMeta (discoveredThisRound attrs + d.count) l'
    EndRound -> do
      l' <- liftRunMessage msg attrs
      pure $ TidalPool $ setMeta (0 :: Int) l'
    _ -> TidalPool <$> liftRunMessage msg attrs
