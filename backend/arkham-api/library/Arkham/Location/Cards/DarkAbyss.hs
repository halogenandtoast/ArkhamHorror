module Arkham.Location.Cards.DarkAbyss (darkAbyss, DarkAbyss (..)) where

import Arkham.Ability
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.FloodLevel
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenario.Types

newtype DarkAbyss = DarkAbyss LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkAbyss :: LocationCard DarkAbyss
darkAbyss = locationWith DarkAbyss Cards.darkAbyss 2 (PerPlayer 1) connectsToAdjacent

instance HasAbilities DarkAbyss where
  getAbilities (DarkAbyss a) =
    extendRevealed
      a
      [ restricted a 1 UnrevealedKeyIsSetAside $ forced $ RevealLocation #after Anyone (be a)
      , restricted a 2 (exists $ orConnected a <> CanHaveFloodLevelIncreased)
          $ forced
          $ DiscoveringLastClue #after Anyone (be a)
      ]

instance RunMessage DarkAbyss where
  runMessage msg l@(DarkAbyss attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      let
        unrevealed = \case
          UnrevealedKey _ -> True
          _ -> False
      unrevealedKeys <- filter unrevealed . setToList <$> scenarioField ScenarioSetAsideKeys
      for_ (nonEmpty unrevealedKeys) $ sample >=> placeKey (toTarget attrs)
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      setThisFloodLevel attrs FullyFlooded
      selectEach (ConnectedTo (be attrs)) increaseThisFloodLevel
      pure l
    _ -> DarkAbyss <$> liftRunMessage msg attrs
