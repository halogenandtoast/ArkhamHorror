module Arkham.Event.Cards.Ambush1 (ambush1, Ambush1 (..)) where

import Arkham.Ability
import Arkham.DamageEffect
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Window (spawnedEnemy)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Placement
import Arkham.Projection

newtype Ambush1 = Ambush1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ambush1 :: EventCard Ambush1
ambush1 = event Ambush1 Cards.ambush1

instance HasAbilities Ambush1 where
  getAbilities (Ambush1 attrs) = case attrs.attachedTo of
    Just (LocationTarget lid) ->
      [ restrictedAbility attrs 1 (exists $ LocationWithId lid <> LocationWithoutInvestigators) Anytime
      , restrictedAbility attrs 2 ControlsThis $ forced $ EnemySpawns #after (LocationWithId lid) AnyEnemy
      ]
    _ -> []

instance RunMessage Ambush1 where
  runMessage msg e@(Ambush1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      lid <- fieldJust InvestigatorLocation iid
      push $ PlaceEvent iid eid (AttachedToLocation lid)
      pure e
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      toDiscardBy attrs.owner (attrs.ability 1) attrs
      pure e
    UseCardAbility _ (isSource attrs -> True) 2 (spawnedEnemy -> enemyId) _ -> do
      canDealDamage <- withoutModifier attrs.owner CannotDealDamage
      let source = attrs.ability 2
      pushWhen canDealDamage $ EnemyDamage enemyId $ nonAttack source 2
      toDiscardBy attrs.owner source attrs
      pure e
    _ -> Ambush1 <$> lift (runMessage msg attrs)
