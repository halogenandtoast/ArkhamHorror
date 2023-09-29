module Arkham.Event.Cards.OnTheHunt (
  onTheHunt,
  OnTheHunt (..),
) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Zone

newtype OnTheHunt = OnTheHunt EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onTheHunt :: EventCard OnTheHunt
onTheHunt = event OnTheHunt Cards.onTheHunt

instance RunMessage OnTheHunt where
  runMessage msg e@(OnTheHunt attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      _ <- popMessageMatching $ \case
        InvestigatorDoDrawEncounterCard iid' -> iid == iid'
        _ -> False
      push
        $ search iid attrs EncounterDeckTarget [(FromTopOfDeck 9, PutBack)] AnyCard (defer $ toTarget attrs)
      pure e
    SearchNoneFound iid (isTarget attrs -> True) -> do
      push $ InvestigatorDrawEncounterCard iid
      pure e
    SearchFound iid (isTarget attrs -> True) _ cards -> do
      additionalTargets <- getAdditionalSearchTargets iid
      let enemyCards = filter (`cardMatch` EnemyType) $ onlyEncounterCards cards
      push
        $ chooseN iid (min (length enemyCards) (1 + additionalTargets))
        $ [ targetLabel (toCardId card) [InvestigatorDrewEncounterCard iid card]
          | card <- enemyCards
          ]
      pure e
    _ -> OnTheHunt <$> runMessage msg attrs
