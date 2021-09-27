module Arkham.Types.Event.Cards.Ambush1
  ( ambush1
  , Ambush1(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Criteria
import Arkham.Types.DamageEffect
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.Matcher hiding (NonAttackDamageEffect)
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Timing qualified as Timing
import Arkham.Types.Window (Window(..))
import Arkham.Types.Window qualified as Window

newtype Ambush1 = Ambush1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env)
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
      , restrictedAbility attrs 2 OwnsThis $ ForcedAbility $ EnemySpawns
        Timing.After
        (LocationWithId lid)
        AnyEnemy
      ]
    _ -> []

instance EventRunner env => RunMessage env Ambush1 where
  runMessage msg e@(Ambush1 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ | eid == toId attrs -> do
      lid <- getId iid
      e <$ push (AttachEvent eid (LocationTarget lid))
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      e <$ push (Discard $ toTarget attrs)
    UseCardAbility iid source [Window _ (Window.EnemySpawns enemyId _)] 2 _
      | isSource attrs source -> e <$ pushAll
        [ EnemyDamage enemyId iid source NonAttackDamageEffect 2
        , Discard $ toTarget attrs
        ]
    _ -> Ambush1 <$> runMessage msg attrs
