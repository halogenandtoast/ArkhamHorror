module Arkham.Event.Cards.StandTogether3 (
  standTogether3,
  StandTogether3 (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher

newtype StandTogether3 = StandTogether3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, Targetable)

standTogether3 :: EventCard StandTogether3
standTogether3 = event StandTogether3 Cards.standTogether3

instance RunMessage StandTogether3 where
  runMessage msg e@(StandTogether3 attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      investigators <- select $ notInvestigator iid <> colocatedWith iid
      player <- getPlayer iid
      choices <- for investigators $ \iid' -> do
        otherDrawing <- drawCardsIfCan iid' (toSource attrs) 2
        gainResources <- gainResourcesIfCan iid' (toSource attrs) 2
        pure (iid', catMaybes [otherDrawing, gainResources])

      youDrawing <- drawCardsIfCan iid (toSource attrs) 2
      youGainResources <- gainResourcesIfCan iid (toSource attrs) 2

      pushAll
        $ [ chooseOrRunOne
            player
            [targetLabel iid' $ msgs <> catMaybes [youDrawing, youGainResources]]
          | (iid', msgs) <- choices
          ]
      pure e
    _ -> StandTogether3 <$> runMessage msg attrs
