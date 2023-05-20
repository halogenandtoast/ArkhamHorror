module Arkham.Event.Cards.SceneOfTheCrime (
  sceneOfTheCrime,
  SceneOfTheCrime (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Investigator
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection

newtype SceneOfTheCrime = SceneOfTheCrime EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sceneOfTheCrime :: EventCard SceneOfTheCrime
sceneOfTheCrime = event SceneOfTheCrime Cards.sceneOfTheCrime

instance RunMessage SceneOfTheCrime where
  runMessage msg e@(SceneOfTheCrime attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      location <- getJustLocation iid
      hasEnemies <- selectAny $ EnemyAt $ LocationWithId location
      availableClues <- field LocationClues location
      let clueCount = min availableClues (if hasEnemies then 2 else 1)
      pushAll
        [ InvestigatorDiscoverClues iid location (toSource attrs) clueCount Nothing
        ]
      pure e
    _ -> SceneOfTheCrime <$> runMessage msg attrs
