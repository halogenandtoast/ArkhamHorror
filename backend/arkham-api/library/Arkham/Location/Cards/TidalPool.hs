module Arkham.Location.Cards.TidalPool (tidalPool, TidalPool (..)) where

import Arkham.Ability
import Arkham.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.FloodLevel
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenario.Types

newtype TidalPool = TidalPool LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tidalPool :: LocationCard TidalPool
tidalPool = locationWith TidalPool Cards.tidalPool 3 (PerPlayer 1) connectsToAdjacent

instance HasAbilities TidalPool where
  getAbilities (TidalPool attrs) =
    extendRevealed
      attrs
      [restricted attrs 1 UnrevealedKeyIsSetAside $ forced $ RevealLocation #after Anyone (be attrs)]

instance HasModifiersFor TidalPool where
  getModifiersFor (TidalPool attrs) = do
    case locationFloodLevel attrs of
      Just FullyFlooded -> modifySelf attrs [ShroudModifier 2]
      Just PartiallyFlooded -> modifySelf attrs [ShroudModifier 1]
      _ -> pure mempty

instance RunMessage TidalPool where
  runMessage msg l@(TidalPool attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      let
        unrevealed = \case
          UnrevealedKey _ -> True
          _ -> False
      unrevealedKeys <- filter unrevealed . setToList <$> scenarioField ScenarioSetAsideKeys
      for_ (nonEmpty unrevealedKeys) $ sample >=> placeKey (toTarget attrs)
      pure l
    _ -> TidalPool <$> liftRunMessage msg attrs
