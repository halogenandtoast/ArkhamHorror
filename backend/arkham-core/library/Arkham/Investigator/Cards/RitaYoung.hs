module Arkham.Investigator.Cards.RitaYoung
  ( ritaYoung
  , ritaYoungElderSignEffect
  , RitaYoung(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Cost
import Arkham.Criteria
import Arkham.DamageEffect
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Game.Helpers
import Arkham.Id
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Message hiding ( EnemyEvaded )
import Arkham.Movement
import Arkham.Source
import Arkham.Timing qualified as Timing
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window

newtype RitaYoung = RitaYoung InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ritaYoung :: InvestigatorCard RitaYoung
ritaYoung = investigator
  RitaYoung
  Cards.ritaYoung
  Stats
    { health = 9
    , sanity = 5
    , willpower = 3
    , intellect = 2
    , combat = 3
    , agility = 5
    }

instance HasAbilities RitaYoung where
  getAbilities (RitaYoung a) =
    [ limitedAbility (PlayerLimit PerRound 1)
        $ restrictedAbility
            a
            1
            (Self <> AnyCriterion
              [ LocationExists AccessibleLocation
              , EnemyCriteria
                (EnemyExists $ EvadingEnemy <> EnemyCanBeDamagedBySource
                  (toAbilitySource a 1)
                )
              ]
            )
        $ ReactionAbility (EnemyEvaded Timing.After You AnyEnemy) Free
    ]

instance HasTokenValue RitaYoung where
  getTokenValue iid ElderSign (RitaYoung attrs) | iid == toId attrs = do
    pure $ TokenValue ElderSign $ PositiveModifier 2
  getTokenValue _ token _ = pure $ TokenValue token mempty

toEnemyId :: [Window] -> EnemyId
toEnemyId [] = error "called outside of expected window"
toEnemyId (x : xs) = case windowType x of
  Window.EnemyEvaded _ eid -> eid
  _ -> toEnemyId xs

instance RunMessage RitaYoung where
  runMessage msg i@(RitaYoung attrs) = case msg of
    ResolveToken _drawnToken ElderSign iid | iid == toId attrs -> do
      push $ createCardEffect Cards.ritaYoung Nothing (toSource attrs) (InvestigatorTarget iid)
      pure i
    UseCardAbility iid (isSource attrs -> True) 1 (toEnemyId -> enemyId) _ ->
      do
        canDamage <- enemyId <=~> EnemyCanBeDamagedBySource (toAbilitySource attrs 1)
        connectingLocations <- selectList AccessibleLocation
        push
          $ chooseOrRunOne iid
          $ [ Label
                "Damage enemy"
                [ EnemyDamage enemyId
                    $ nonAttack (toAbilitySource attrs 1) 1
                ]
            | canDamage
            ]
          <> [ Label
                 "Move to a connecting location"
                 [ chooseOne
                     iid
                     [ TargetLabel
                         (LocationTarget lid')
                         [Move $ move (toSource attrs) iid lid']
                     | lid' <- connectingLocations
                     ]
                 ]
             | notNull connectingLocations
             ]
        pure i
    _ -> RitaYoung <$> runMessage msg attrs

newtype RitaYoungElderSignEffect = RitaYoungElderSignEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ritaYoungElderSignEffect :: EffectArgs -> RitaYoungElderSignEffect
ritaYoungElderSignEffect = cardEffect RitaYoungElderSignEffect Cards.ritaYoung

instance HasModifiersFor RitaYoungElderSignEffect where
  getModifiersFor (AbilityTarget iid ab) (RitaYoungElderSignEffect a)
    | abilityIndex ab == 1 && abilitySource ab == InvestigatorSource iid && InvestigatorTarget iid == effectTarget a = do
      pure $ toModifiers a [IgnoreLimit]
  getModifiersFor _ _ = pure []

instance RunMessage RitaYoungElderSignEffect where
  runMessage msg e@(RitaYoungElderSignEffect attrs@EffectAttrs {..}) =
    case msg of
      EndRound -> do
        push (DisableEffect effectId)
        pure e
      _ -> RitaYoungElderSignEffect <$> runMessage msg attrs
