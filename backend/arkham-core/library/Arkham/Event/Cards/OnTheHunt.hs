module Arkham.Event.Cards.OnTheHunt (onTheHunt, OnTheHunt (..)) where

import Arkham.Card
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Message (drawEncounterCard)
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Modifiers qualified as Msg
import Arkham.Matcher
import Arkham.Spawn
import Arkham.Strategy
import Arkham.Zone

newtype OnTheHunt = OnTheHunt EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onTheHunt :: EventCard OnTheHunt
onTheHunt = event OnTheHunt Cards.onTheHunt

instance RunMessage OnTheHunt where
  runMessage msg e@(OnTheHunt attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      insteadOf (InvestigatorDoDrawEncounterCard iid) (pure ())
      search iid attrs EncounterDeckTarget [(FromTopOfDeck 9, PutBack)] AnyCard (defer attrs)
      pure e
    SearchNoneFound iid (isTarget attrs -> True) -> do
      push $ drawEncounterCard iid attrs
      pure e
    SearchFound iid (isTarget attrs -> True) _ cards -> do
      additionalTargets <- getAdditionalSearchTargets iid
      let enemyCards = filter (`cardMatch` EnemyType) $ onlyEncounterCards cards
      chooseN iid (min (length enemyCards) (1 + additionalTargets))
        $ [ targetLabel
            (toCardId card)
            [ Msg.searchModifier
                attrs
                (CardIdTarget $ toCardId card)
                (ForceSpawn (SpawnEngagedWith $ InvestigatorWithId iid))
            , InvestigatorDrewEncounterCard iid card
            ]
          | card <- enemyCards
          ]
      pure e
    _ -> OnTheHunt <$> lift (runMessage msg attrs)
