module Arkham.Event.Cards.StandTogether (
  standTogether,
  StandTogether (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher

newtype StandTogether = StandTogether EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

standTogether :: EventCard StandTogether
standTogether = event StandTogether Cards.standTogether

instance RunMessage StandTogether where
  runMessage msg e@(StandTogether attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      investigators <- select $ notInvestigator iid <> colocatedWith iid
      player <- getPlayer iid
      choices <- for investigators $ \iid' -> do
        gainResources <- gainResourcesIfCan iid' (toSource attrs) 2
        pure (iid', maybeToList gainResources)

      youGainResources <- gainResourcesIfCan iid (toSource attrs) 2

      pushAll
        $ [ chooseOrRunOne
            player
            [targetLabel iid' $ msgs <> maybeToList youGainResources]
          | (iid', msgs) <- choices
          ]
      pure e
    _ -> StandTogether <$> runMessage msg attrs
