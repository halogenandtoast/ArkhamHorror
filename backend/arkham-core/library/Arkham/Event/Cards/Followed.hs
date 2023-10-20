module Arkham.Event.Cards.Followed (
  followed,
  followedEffect,
  Followed (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Enemy.Types qualified as Field
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Investigate
import Arkham.Matcher

newtype Followed = Followed EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

followed :: EventCard Followed
followed = event Followed Cards.followed

instance RunMessage Followed where
  runMessage msg e@(Followed attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      investigation <- mkInvestigate iid attrs
      push $ toMessage investigation
      pure e
    _ -> Followed <$> runMessage msg attrs

newtype FollowedEffect = FollowedEffect EffectAttrs
  deriving anyclass (HasAbilities, HasModifiersFor, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

followedEffect :: EffectArgs -> FollowedEffect
followedEffect = cardEffect FollowedEffect Cards.followed

instance RunMessage FollowedEffect where
  runMessage msg e@(FollowedEffect attrs) = case msg of
    CreatedEffect eid _ (InvestigatorSource iid) _ | eid == toId attrs -> do
      enemies <- selectWithField Field.EnemyDamage $ enemyAtLocationWith iid
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ targetLabel
            enemy
            [ cardResolutionModifier (toSource attrs) enemy CannotMakeAttacksOfOpportunity
            , skillTestModifiers (toSource attrs) iid [SkillModifier #intellect (min 5 dmg), DiscoveredClues 1]
            , disable attrs
            ]
          | (enemy, dmg) <- enemies
          ]
      pure e
    _ -> FollowedEffect <$> runMessage msg attrs
