module Arkham.Event.Cards.SceneOfTheCrime (sceneOfTheCrime, SceneOfTheCrime (..)) where

import Arkham.Classes
import Arkham.Discover
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Prelude

newtype SceneOfTheCrime = SceneOfTheCrime EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sceneOfTheCrime :: EventCard SceneOfTheCrime
sceneOfTheCrime = event SceneOfTheCrime Cards.sceneOfTheCrime

instance RunMessage SceneOfTheCrime where
  runMessage msg e@(SceneOfTheCrime attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      hasEnemies <- selectAny $ enemyAtLocationWith iid
      let clueCount = if hasEnemies then 2 else 1
      push $ Msg.DiscoverClues iid $ discoverAtYourLocation attrs clueCount
      pure e
    _ -> SceneOfTheCrime <$> runMessage msg attrs
