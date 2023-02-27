module Arkham.Event.Cards.OnTheHunt
  ( onTheHunt
  , OnTheHunt(..)
  ) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Zone

newtype OnTheHunt = OnTheHunt EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onTheHunt :: EventCard OnTheHunt
onTheHunt = event OnTheHunt Cards.onTheHunt

instance RunMessage OnTheHunt where
  runMessage msg e@(OnTheHunt attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      _ <- popMessageMatching $ \case
        InvestigatorDoDrawEncounterCard iid' -> iid == iid'
        _ -> False
      push $ Search
        iid
        (toSource attrs)
        EncounterDeckTarget
        [(FromTopOfDeck 9, PutBack)]
        AnyCard
        (DeferSearchedToTarget $ toTarget attrs)
      pure e
    SearchNoneFound iid (isTarget attrs -> True) -> do
      push $ InvestigatorDrawEncounterCard iid
      pure e
    SearchFound iid (isTarget attrs -> True) _ cards -> do
      let
        enemyCards =
          filter ((== EnemyType) . toCardType)
            $ mapMaybe (preview _EncounterCard) cards
      push $ chooseOne
        iid
        [ TargetLabel
            (CardIdTarget $ toCardId card)
            [InvestigatorDrewEncounterCard iid card]
        | card <- enemyCards
        ]
      pure e
    _ -> OnTheHunt <$> runMessage msg attrs
