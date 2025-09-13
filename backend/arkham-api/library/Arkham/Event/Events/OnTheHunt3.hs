module Arkham.Event.Events.OnTheHunt3 (onTheHunt3) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Card
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted hiding (EnemyDefeated)
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.Search
import Arkham.Matcher
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
      [ controlled attrs 1 (youExist can.gain.resources)
          $ freeReaction (EnemyDefeated #when You ByAny $ EnemyWithId enemy)
      ]
    _ -> []

instance RunMessage OnTheHunt3 where
  runMessage msg e@(OnTheHunt3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      gainResources iid (attrs.ability 1) 3
      pure e
    PlayThisEvent iid (is attrs -> True) -> do
      don't $ DoDrawCards iid
      search iid attrs EncounterDeckTarget [(FromDeck, PutBack)] #any (defer attrs IsDraw)
      pure e
    SearchFound iid (isTarget attrs -> True) _ cards -> do
      focusCards cards do
        chooseFromSearch iid 1 (filterCards EnemyType cards) \card -> do
          searchModifiers attrs card [ForceSpawn (SpawnEngagedWith $ be iid), IgnoreRevelation]
          drawCard iid card
          attach attrs.id card
      pure e
    _ -> OnTheHunt3 <$> liftRunMessage msg attrs
