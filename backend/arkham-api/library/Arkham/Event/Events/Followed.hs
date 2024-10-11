module Arkham.Event.Events.Followed (followed, followedEffect, Followed (..)) where

import Arkham.Card
import Arkham.Effect.Import
import Arkham.Enemy.Types qualified as Field
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import {-# SOURCE #-} Arkham.GameEnv (getCard)
import Arkham.Helpers.Modifiers (ModifierType (..), getMeta)
import Arkham.Matcher

newtype Followed = Followed EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

followed :: EventCard Followed
followed = event Followed Cards.followed

instance RunMessage Followed where
  runMessage msg e@(Followed attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == attrs.id -> do
      sid <- getRandom
      enemy <- fromJustNote "damage should be set" <$> getMeta (toCardId attrs) "enemy"
      skillTestModifiers
        sid
        attrs
        iid
        [ ForEach
            (MaxCalculation (Fixed 5) $ EnemyFieldCalculation enemy Field.EnemyDamage)
            [SkillModifier #intellect 1]
        , DiscoveredClues 1
        ]
      investigate sid iid attrs
      pure e
    _ -> Followed <$> liftRunMessage msg attrs

newtype FollowedEffect = FollowedEffect EffectAttrs
  deriving anyclass (HasAbilities, HasModifiersFor, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

followedEffect :: EffectArgs -> FollowedEffect
followedEffect = cardEffect FollowedEffect Cards.followed

-- effect is triggered by cdBeforeEffect
instance RunMessage FollowedEffect where
  runMessage msg e@(FollowedEffect attrs) = runQueueT $ case msg of
    CreatedEffect eid _ (BothSource (InvestigatorSource iid) _) target | eid == toId attrs -> do
      enemies <- select $ enemyAtLocationWith iid
      card <- case attrs.target of
        CardIdTarget cid -> getCard cid
        _ -> error "FollowedEffect: cardId should be CardIdTarget"
      chooseTargetM iid enemies \enemy -> do
        costModifier attrs enemy CannotMakeAttacksOfOpportunity
        cardResolutionModifier card attrs target (MetaModifier $ object ["enemy" .= enemy])
        disable attrs
      pure e
    _ -> FollowedEffect <$> liftRunMessage msg attrs
