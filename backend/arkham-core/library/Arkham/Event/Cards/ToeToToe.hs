module Arkham.Event.Cards.ToeToToe (
  toeToToe,
  toeToToeEffect,
  ToeToToe (..),
)
where

import Arkham.Card
import Arkham.Classes
import Arkham.Cost
import Arkham.Effect.Runner
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Fight
import {-# SOURCE #-} Arkham.GameEnv (getCard)
import Arkham.Helpers.Modifiers
import Arkham.Prelude

newtype ToeToToe = ToeToToe EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

toeToToe :: EventCard ToeToToe
toeToToe = event ToeToToe Cards.toeToToe

instance RunMessage ToeToToe where
  runMessage msg e@(ToeToToe attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      enemy <- fromJustNote "enemy should be set" <$> getMeta (toCardId attrs) "chosenEnemy"
      pushAll
        [ skillTestModifiers attrs iid [DamageDealt 1, SkillTestAutomaticallySucceeds]
        , FightEnemy iid enemy (toSource attrs) Nothing #combat False
        ]
      pure e
    _ -> ToeToToe <$> runMessage msg attrs

newtype ToeToToeEffect = ToeToToeEffect EffectAttrs
  deriving anyclass (HasAbilities, HasModifiersFor, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

toeToToeEffect :: EffectArgs -> ToeToToeEffect
toeToToeEffect = cardEffect ToeToToeEffect Cards.toeToToe

-- effect is triggered by cdBeforeEffect
instance RunMessage ToeToToeEffect where
  runMessage msg e@(ToeToToeEffect attrs) = case msg of
    CreatedEffect eid _ (InvestigatorSource iid) _target | eid == toId attrs -> do
      pushM $ toMessage . setTarget attrs <$> onlyChooseFight (mkChooseFight iid (toSource attrs))
      pure e
    ChoseEnemy sid _iid (isSource attrs -> True) enemy -> do
      case attrs.meta of
        Just (EffectCost acId) -> do
          card <- case attrs.target of
            CardIdTarget cid -> getCard cid
            _ -> error "ToeToToeEffect: cardId should be CardIdTarget"
          pushAll
            [ disable attrs
            , costModifier attrs (ActiveCostTarget acId) (AdditionalCost $ EnemyAttackCost enemy)
            , cardResolutionModifier card attrs attrs.target (MetaModifier $ object ["chosenEnemy" .= enemy])
            ]
        _ -> error "invalid before effect meta"
      pure e
    _ -> ToeToToeEffect <$> runMessage msg attrs
