module Arkham.Event.Cards.ActOfDesperation (
  actOfDesperation,
  actOfDesperationEffect,
  ActOfDesperation (..),
) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Cost
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.EffectMetadata
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers

newtype ActOfDesperation = ActOfDesperation EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

actOfDesperation :: EventCard ActOfDesperation
actOfDesperation = event ActOfDesperation Cards.actOfDesperation

getDiscards :: Payment -> [(Zone, Card)]
getDiscards (DiscardPayment c) = c
getDiscards (Payments ps) = concatMap getDiscards ps
getDiscards _ = []

instance RunMessage ActOfDesperation where
  runMessage msg e@(ActOfDesperation attrs) = case msg of
    PlayThisEvent iid eid | attrs `is` eid -> do
      case getDiscards attrs.payment of
        [(zone, discard)] -> do
          let n = maybe 0 toPrintedCost . cdCost $ toCardDef discard
          pushAll
            $ skillTestModifiers attrs iid (DamageDealt 1 : [SkillModifier #combat n | n > 0])
            : [ createCardEffect Cards.actOfDesperation (Just (EffectInt n)) attrs iid | zone == FromPlay && n > 0
              ]
              <> [chooseFightEnemy iid attrs #combat]
        _ -> error "Invalid choice"
      pure e
    _ -> ActOfDesperation <$> runMessage msg attrs

newtype ActOfDesperationEffect = ActOfDesperationEffect EffectAttrs
  deriving anyclass (HasAbilities, HasModifiersFor, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

actOfDesperationEffect :: EffectArgs -> ActOfDesperationEffect
actOfDesperationEffect =
  cardEffect ActOfDesperationEffect Cards.actOfDesperation

instance RunMessage ActOfDesperationEffect where
  runMessage msg e@(ActOfDesperationEffect attrs@EffectAttrs {..}) =
    case msg of
      PassedThisSkillTest _ source | source == effectSource -> do
        case (effectMetadata, effectTarget) of
          (Just (EffectInt n), InvestigatorTarget iid) ->
            pushAll [TakeResources iid n effectSource False, DisableEffect effectId]
          _ -> error "Invalid call"
        pure e
      SkillTestEnds _ _ -> do
        push $ DisableEffect effectId
        pure e
      _ -> ActOfDesperationEffect <$> runMessage msg attrs
