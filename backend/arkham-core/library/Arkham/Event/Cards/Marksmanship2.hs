module Arkham.Event.Cards.Marksmanship2
  ( marksmanship2
  , marksmanship2Effect
  , Marksmanship2(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Card
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Game.Helpers
import Arkham.Helpers.SkillTest
import Arkham.Matcher hiding ( EventCard )
import Arkham.Message
import Arkham.Source
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Trait
import Arkham.Window

-- This card is a bit complicated since it changes targetting rules, we have to
-- allow the player to use an ability on an asset when they normally couldn't
-- when this card is in their hand, hence the long list of checks in
-- `HasModifiersFor`. In addition to being able to use the effect, we need to
-- make sure the card is playable outside of it's normal window. At the moment
-- we use `DoNotCheckWindow` which is intended to be an always valid window.

-- Once we've confirmed that Marksmanship2s effect should happen, we need to
-- check if any enemies are now fightable using the normal rules, but a
-- different criteria, so we use an override system to do that. If there are
-- other game rules that affect this criteria we will need to make sure the
-- override happens first.

-- For more info, see the comments before for the Effect

newtype Marksmanship2 = Marksmanship2 EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

marksmanship2 :: EventCard Marksmanship2
marksmanship2 = event Marksmanship2 Cards.marksmanship2

instance HasModifiersFor Marksmanship2 where
  getModifiersFor (AbilityTarget iid ability) (Marksmanship2 a)
    | eventOwner a == iid = case abilityAction ability of
      Just Action.Fight -> do
        traits <- sourceTraits (abilitySource ability)
        if any (`elem` traits) [Firearm, Ranged]
          then do
            mlid <- selectOne $ locationWithInvestigator iid
            case mlid of
              Nothing -> pure []
              Just lid -> do
                isPlayable <- getIsPlayable
                  iid
                  (InvestigatorSource iid)
                  UnpaidCost
                  [Window Timing.When DoNotCheckWindow]
                  (toCard a)
                pure $ toModifiers
                  a
                  [ EnemyFightActionCriteria
                    $ CriteriaOverride
                    $ EnemyCriteria
                    $ ThisEnemy
                    $ EnemyWithoutModifier CannotBeAttacked
                    <> EnemyAt
                         (LocationMatchAny
                           [ LocationWithId lid
                           , ConnectedTo $ LocationWithId lid
                           ]
                         )
                  | isPlayable
                  ]
          else pure []
      _ -> pure []
  getModifiersFor _ _ = pure []


instance RunMessage Marksmanship2 where
  runMessage msg e@(Marksmanship2 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      pushAll
        [ createCardEffect
          Cards.marksmanship2
          Nothing
          (toSource attrs)
          (InvestigatorTarget iid)
        , Discard (toTarget attrs)
        ]
      pure e
    _ -> Marksmanship2 <$> runMessage msg attrs


newtype Marksmanship2Effect = Marksmanship2Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

marksmanship2Effect :: EffectArgs -> Marksmanship2Effect
marksmanship2Effect = cardEffect Marksmanship2Effect Cards.marksmanship2

-- Once marksmanship2 has been played this effect portion is a little easier,
-- we still have to replace the criteria, but since we don't have a specific
-- enemy, we add this modifier to all enemies, and in order to have it only be
-- valid during targetting, we disable it as soon as the fight enemy message is
-- processed.

-- Additionally since there are effects that touch different things, we
-- "swizzle" the target in order to disable/enable to appropriate effects

instance HasModifiersFor Marksmanship2Effect where
  getModifiersFor (EnemyTarget eid) (Marksmanship2Effect a) =
    case effectTarget a of
      InvestigatorTarget _ -> pure $ toModifiers
        a
        [ EnemyFightActionCriteria
          $ CriteriaOverride
          $ AnyCriterion
              [OnSameLocation, OnLocation $ ConnectedTo $ locationWithEnemy eid]
          <> EnemyCriteria (ThisEnemy $ EnemyWithoutModifier CannotBeAttacked)
        ]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage Marksmanship2Effect where
  runMessage msg e@(Marksmanship2Effect attrs@EffectAttrs {..}) = case msg of
    FightEnemy iid eid _ _ _ _ -> do
      push $ skillTestModifiers
        (toSource attrs)
        (InvestigatorTarget iid)
        [IgnoreRetaliate]
      pure . Marksmanship2Effect $ attrs & targetL .~ EnemyTarget eid
    PassedSkillTest iid (Just Action.Fight) _ SkillTestInitiatorTarget{} _ _ ->
      do
        mSkillTestTarget <- getSkillTestTarget
        for_ mSkillTestTarget $ \case
          target@(EnemyTarget eid) | effectTarget == target -> do
            engaged <- eid <=~> enemyEngagedWith iid
            unless engaged $ push $ skillTestModifier
              attrs
              (InvestigatorTarget iid)
              (DamageDealt 1)
          _ -> pure ()
        pure e
    SkillTestEnds _ -> do
      push $ DisableEffect effectId
      pure e
    _ -> Marksmanship2Effect <$> runMessage msg attrs
