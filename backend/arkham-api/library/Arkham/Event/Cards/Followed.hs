module Arkham.Event.Cards.Followed (followed, followedEffect, Followed (..)) where

import Arkham.Card
import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Enemy.Types qualified as Field
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import {-# SOURCE #-} Arkham.GameEnv (getCard)
import Arkham.Helpers.Modifiers
import Arkham.Investigate
import Arkham.Matcher
import Arkham.Prelude

newtype Followed = Followed EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

followed :: EventCard Followed
followed = event Followed Cards.followed

instance RunMessage Followed where
  runMessage msg e@(Followed attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      sid <- getRandom
      investigation <- mkInvestigate sid iid attrs
      enemy <- fromJustNote "damage should be set" <$> getMeta (toCardId attrs) "enemy"

      pushAll
        [ skillTestModifiers
            sid
            attrs
            iid
            [ ForEach
                (MaxCalculation (Fixed 5) $ EnemyFieldCalculation enemy Field.EnemyDamage)
                [SkillModifier #intellect 1]
            , DiscoveredClues 1
            ]
        , toMessage investigation
        ]
      pure e
    _ -> Followed <$> runMessage msg attrs

newtype FollowedEffect = FollowedEffect EffectAttrs
  deriving anyclass (HasAbilities, HasModifiersFor, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

followedEffect :: EffectArgs -> FollowedEffect
followedEffect = cardEffect FollowedEffect Cards.followed

-- effect is triggered by cdBeforeEffect
instance RunMessage FollowedEffect where
  runMessage msg e@(FollowedEffect attrs) = case msg of
    CreatedEffect eid _ (BothSource (InvestigatorSource iid) _) target | eid == toId attrs -> do
      enemies <- select $ enemyAtLocationWith iid
      player <- getPlayer iid
      card <- case attrs.target of
        CardIdTarget cid -> getCard cid
        _ -> error "FollowedEffect: cardId should be CardIdTarget"
      push
        $ chooseOne
          player
          [ targetLabel
            enemy
            [ costModifier attrs enemy CannotMakeAttacksOfOpportunity
            , cardResolutionModifier card attrs target (MetaModifier $ object ["enemy" .= enemy])
            , disable attrs
            ]
          | enemy <- enemies
          ]
      pure e
    _ -> FollowedEffect <$> runMessage msg attrs
