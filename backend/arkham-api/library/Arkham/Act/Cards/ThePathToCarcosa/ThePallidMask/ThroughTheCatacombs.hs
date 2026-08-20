module Arkham.Act.Cards.ThePathToCarcosa.ThePallidMask.ThroughTheCatacombs (throughTheCatacombs) where

import Arkham.Act.CardDefs.ThePathToCarcosa.ThePallidMask qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.ThePathToCarcosa.Helpers
import Arkham.Enemy.CardDefs.ThePathToCarcosa.CurtainCall qualified as Enemies
import Arkham.Enemy.CardDefs.ThePathToCarcosa.TheLastKing qualified as Enemies
import Arkham.Location.CardDefs.ThePathToCarcosa.ThePallidMask qualified as Locations
import Arkham.Matcher
import Arkham.Scenarios.ThePathToCarcosa.ThePallidMask.Helpers

newtype ThroughTheCatacombs = ThroughTheCatacombs ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

throughTheCatacombs :: ActCard ThroughTheCatacombs
throughTheCatacombs = act (1, A) ThroughTheCatacombs Cards.throughTheCatacombs Nothing

instance RunMessage ThroughTheCatacombs where
  runMessage msg a@(ThroughTheCatacombs attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      tombOfShadows <- selectJust $ locationIs Locations.tombOfShadows
      createEnemyAt_ Enemies.theManInThePallidMask tombOfShadows
      spawnIshimaruHaruko <- not <$> slain Enemies.ishimaruHaruko
      when spawnIshimaruHaruko $ createEnemyAt_ Enemies.ishimaruHaruko =<< getStartingLocation
      advanceActDeck attrs
      pure a
    _ -> ThroughTheCatacombs <$> liftRunMessage msg attrs
