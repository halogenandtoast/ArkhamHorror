module Arkham.Event.Cards.OnTheHunt3 (onTheHunt3, OnTheHunt3 (..)) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Card
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted hiding (EnemyDefeated)
import Arkham.Helpers.Message (drawEncounterCard)
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Modifiers qualified as Msg
import Arkham.Matcher
import Arkham.Placement
import Arkham.Spawn
import Arkham.Strategy

newtype OnTheHunt3 = OnTheHunt3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onTheHunt3 :: EventCard OnTheHunt3
onTheHunt3 = event OnTheHunt3 Cards.onTheHunt3

instance HasAbilities OnTheHunt3 where
  getAbilities (OnTheHunt3 attrs) = case attrs.placement of
    AttachedToEnemy enemy ->
      [ controlledAbility attrs 1 (youExist can.gain.resources)
          $ ReactionAbility (EnemyDefeated #when You ByAny $ EnemyWithId enemy) Free
      ]
    _ -> []

instance RunMessage OnTheHunt3 where
  runMessage msg e@(OnTheHunt3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      gainResourcesIfCan iid (attrs.ability 1) 3
      pure e
    PlayThisEvent iid eid | eid == toId attrs -> do
      insteadOf (DoDrawCards iid) (pure ())
      search iid attrs EncounterDeckTarget [(FromDeck, PutBack)] AnyCard (defer attrs)
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
            , AttachEvent (toId e) (CardIdTarget $ toCardId card)
            ]
          | card <- enemyCards
          ]
      pure e
    _ -> OnTheHunt3 <$> lift (runMessage msg attrs)
