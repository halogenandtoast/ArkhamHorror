module Arkham.Event.Cards.Ambush1 (
  ambush1,
  Ambush1 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.DamageEffect
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Placement
import Arkham.Projection
import Arkham.Timing qualified as Timing
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype Ambush1 = Ambush1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ambush1 :: EventCard Ambush1
ambush1 = event Ambush1 Cards.ambush1

instance HasAbilities Ambush1 where
  getAbilities (Ambush1 attrs) = case eventAttachedTarget attrs of
    Just (LocationTarget lid) ->
      [ restrictedAbility
          attrs
          1
          (LocationExists $ LocationWithId lid <> LocationWithoutInvestigators)
          $ SilentForcedAbility AnyWindow
      , restrictedAbility attrs 2 ControlsThis
          $ ForcedAbility
          $ EnemySpawns
            Timing.After
            (LocationWithId lid)
            AnyEnemy
      ]
    _ -> []

instance RunMessage Ambush1 where
  runMessage msg e@(Ambush1 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      lid <- fieldJust InvestigatorLocation iid
      push $ PlaceEvent iid eid (AttachedToLocation lid)
      pure e
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      push $ Discard (toAbilitySource attrs 1) (toTarget attrs)
      pure e
    UseCardAbility _ source 2 [(windowType -> Window.EnemySpawns enemyId _)] _ | isSource attrs source -> do
      iid <- field EventOwner (toId attrs)
      canDealDamage <- withoutModifier iid CannotDealDamage
      pushAll
        $ [EnemyDamage enemyId $ nonAttack source 2 | canDealDamage]
        <> [ Discard (toAbilitySource attrs 2) (toTarget attrs)
           ]
      pure e
    _ -> Ambush1 <$> runMessage msg attrs
