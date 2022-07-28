module Arkham.Act.Cards.ThroughTheCatacombs
  ( ThroughTheCatacombs(..)
  , throughTheCatacombs
  ) where

import Arkham.Prelude

import Arkham.Act.Types
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Helpers
import Arkham.Act.Runner
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message
import Arkham.Scenarios.ThePallidMask.Helpers

newtype ThroughTheCatacombs = ThroughTheCatacombs ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

throughTheCatacombs :: ActCard ThroughTheCatacombs
throughTheCatacombs =
  act (1, A) ThroughTheCatacombs Cards.throughTheCatacombs Nothing

instance RunMessage ThroughTheCatacombs where
  runMessage msg a@(ThroughTheCatacombs attrs) = case msg of
    AdvanceAct aid _ _ | aid == actId attrs && onSide B attrs -> do
      theManInThePallidMask <- getSetAsideCard Enemies.theManInThePallidMask
      startingLocation <- getStartingLocation
      tombOfShadows <- selectJust $ locationIs Locations.tombOfShadows
      spawnIshimaruHaruko <-
        notElem (Recorded $ toCardCode Enemies.ishimaruHaruko)
          <$> getRecordSet VIPsSlain
      spawnIshimaruHarukoMessages <- if spawnIshimaruHaruko
        then do
          card <- genCard Enemies.ishimaruHaruko
          pure
            [ CreateEnemyAtLocationMatching
                card
                (LocationWithId startingLocation)
            ]
        else pure []
      pushAll
        ([CreateEnemyAt theManInThePallidMask tombOfShadows Nothing]
        <> spawnIshimaruHarukoMessages
        <> [AdvanceActDeck (actDeckId attrs) (toSource attrs)]
        )
      pure a
    _ -> ThroughTheCatacombs <$> runMessage msg attrs
