module Arkham.Act.Cards.ThroughTheCatacombs (
  ThroughTheCatacombs (..),
  throughTheCatacombs,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Helpers
import Arkham.Act.Runner
import Arkham.Campaigns.ThePathToCarcosa.Helpers
import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Scenarios.ThePallidMask.Helpers

newtype ThroughTheCatacombs = ThroughTheCatacombs ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, HasAbilities)

throughTheCatacombs :: ActCard ThroughTheCatacombs
throughTheCatacombs =
  act (1, A) ThroughTheCatacombs Cards.throughTheCatacombs Nothing

instance RunMessage ThroughTheCatacombs where
  runMessage msg a@(ThroughTheCatacombs attrs) = case msg of
    AdvanceAct aid _ _ | aid == actId attrs && onSide B attrs -> do
      theManInThePallidMask <- getSetAsideCard Enemies.theManInThePallidMask
      startingLocation <- getStartingLocation
      tombOfShadows <- selectJust $ locationIs Locations.tombOfShadows
      spawnIshimaruHarukoMessages <- do
        spawnIshimaruHaruko <- not <$> slain Enemies.ishimaruHaruko
        card <- genCard Enemies.ishimaruHaruko
        createIshimaruHaruko <-
          createEnemyAtLocationMatching_
            card
            (LocationWithId startingLocation)
        pure [createIshimaruHaruko | spawnIshimaruHaruko]

      createTheManInThePallidMask <-
        createEnemyAt_
          theManInThePallidMask
          tombOfShadows
          Nothing

      pushAll
        $ createTheManInThePallidMask
        : spawnIshimaruHarukoMessages
          <> [AdvanceActDeck (actDeckId attrs) (toSource attrs)]
      pure a
    _ -> ThroughTheCatacombs <$> runMessage msg attrs
