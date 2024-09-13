module Arkham.Act.Cards.ThePit (ThePit (..), thePit) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheInnsmouthConspiracy.Memory
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Location.Grid
import Arkham.Matcher
import Arkham.Scenario.Types (Field (..))
import Arkham.Treachery.Cards qualified as Treacheries

newtype ThePit = ThePit ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

thePit :: ActCard ThePit
thePit = act (1, A) ThePit Cards.thePit (groupClueCost $ Static 3)

instance RunMessage ThePit where
  runMessage msg a@(ThePit attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      lead <- getLead
      createSetAsideEnemy Enemies.theAmalgam lead
      shuffleSetAsideIntoEncounterDeck
        $ oneOf [cardIs Treacheries.blindsense, cardIs Treacheries.fromTheDepths]

      tidalTunnelDeck <- shuffleM =<< getSetAsideCardsMatching "Tidal Tunnel"
      revealedLocations <- select RevealedLocation
      grid <- scenarioField ScenarioGrid

      let
        locationPositions lid = case findInGrid lid grid of
          Nothing -> []
          Just (Pos x y) -> filter (\pos -> isNothing $ viewGrid pos grid) [Pos x (y - 1), Pos (x - 1) y, Pos (x + 1) y]

      let positions = nub $ concatMap locationPositions revealedLocations
      for_ (zip positions tidalTunnelDeck) (uncurry placeLocationInGrid)

      story $ i18nWithTitle "theInnsmouthConspiracy.thePitOfDespair.flashback1"
      recordSetInsert MemoriesRecovered [AMeetingWithThomasDawson]

      advanceActDeck attrs
      pure a
    _ -> ThePit <$> liftRunMessage msg attrs
