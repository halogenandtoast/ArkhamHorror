module Arkham.Event.Cards.Marksmanship1 (
  marksmanship1,
  marksmanship1Effect,
  Marksmanship1 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Card
import Arkham.Classes
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner hiding (targetL)
import Arkham.Game.Helpers
import Arkham.Keyword (Keyword (Aloof, Retaliate))
import Arkham.Matcher hiding (EventCard)
import Arkham.Timing qualified as Timing
import Arkham.Trait
import Arkham.Window
import Arkham.Window qualified as Window

-- This card is a bit complicated since it changes targetting rules, we have to
-- allow the player to use an ability on an asset when they normally couldn't
-- when this card is in their hand, hence the long list of checks in
-- `HasModifiersFor`. In addition to being able to use the effect, we need to
-- make sure the card is playable outside of it's normal window. At the moment
-- we use `DoNotCheckWindow` which is intended to be an always valid window.

-- Once we've confirmed that Marksmanship1s effect should happen, we need to
-- check if any enemies are now fightable using the normal rules, but a
-- different criteria, so we use an override system to do that. If there are
-- other game rules that affect this criteria we will need to make sure the
-- override happens first.

-- For more info, see the comments before for the Effect

newtype Marksmanship1 = Marksmanship1 EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

marksmanship1 :: EventCard Marksmanship1
marksmanship1 = event Marksmanship1 Cards.marksmanship1

instance HasModifiersFor Marksmanship1 where
  getModifiersFor (AbilityTarget iid ability) (Marksmanship1 a)
    | eventOwner a == iid =
        if Action.Fight `elem` abilityActions ability
          then do
            traits <- sourceTraits (abilitySource ability)
            if any (`elem` traits) [Firearm, Ranged]
              then do
                mlid <- selectOne $ locationWithInvestigator iid
                case mlid of
                  Nothing -> pure []
                  Just lid -> do
                    isPlayable <-
                      getIsPlayable
                        iid
                        (InvestigatorSource iid)
                        (UnpaidCost NeedsAction)
                        [mkWindow Timing.When DoNotCheckWindow]
                        (toCard a)
                    pure
                      $ toModifiers
                        a
                        [ CanModify
                          $ EnemyFightActionCriteria
                          $ CriteriaOverride
                          $ EnemyCriteria
                          $ ThisEnemy
                          $ EnemyWithoutModifier CannotBeAttacked
                          <> EnemyAt
                            ( LocationMatchAny
                                [ LocationWithId lid
                                , ConnectedTo $ LocationWithId lid
                                ]
                            )
                        | isPlayable
                        ]
              else pure []
          else pure []
  getModifiersFor _ _ = pure []

instance RunMessage Marksmanship1 where
  runMessage msg e@(Marksmanship1 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      pushAll [createCardEffect Cards.marksmanship1 Nothing attrs iid]
      pure e
    _ -> Marksmanship1 <$> runMessage msg attrs

newtype Marksmanship1Effect = Marksmanship1Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

marksmanship1Effect :: EffectArgs -> Marksmanship1Effect
marksmanship1Effect = cardEffect Marksmanship1Effect Cards.marksmanship1

-- Once marksmanship1 has been played this effect portion is a little easier,
-- we still have to replace the criteria, but since we don't have a specific
-- enemy, we add this modifier to all enemies, and in order to have it only be
-- valid during targetting, we disable it as soon as the fight enemy message is
-- processed.

-- Additionally since there are effects that touch different things, we
-- "swizzle" the target in order to disable/enable to appropriate effects

instance HasModifiersFor Marksmanship1Effect where
  getModifiersFor (EnemyTarget eid) (Marksmanship1Effect a) =
    case effectTarget a of
      InvestigatorTarget _ ->
        pure
          $ toModifiers
            a
            [ EnemyFightActionCriteria
                $ CriteriaOverride
                $ AnyCriterion
                  [OnSameLocation, OnLocation $ ConnectedTo $ locationWithEnemy eid]
                <> EnemyCriteria (ThisEnemy $ EnemyWithoutModifier CannotBeAttacked)
            ]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage Marksmanship1Effect where
  runMessage msg e@(Marksmanship1Effect attrs@EffectAttrs {..}) = case msg of
    FightEnemy sid iid eid _ _ _ _ -> do
      ignored <-
        selectAny $ EnemyWithId eid <> EnemyOneOf [EnemyWithKeyword Retaliate, EnemyWithKeyword Aloof]
      ignoreWindow <-
        checkWindows [mkWindow Timing.After (Window.CancelledOrIgnoredCardOrGameEffect effectSource)]
      pushAll
        $ skillTestModifiers
          sid
          (toSource attrs)
          (InvestigatorTarget iid)
          [IgnoreRetaliate, IgnoreAloof]
        : [ignoreWindow | ignored]
      pure . Marksmanship1Effect $ attrs & targetL .~ EnemyTarget eid
    PassedSkillTest iid (Just Action.Fight) _ SkillTestInitiatorTarget {} _ _ ->
      do
        mSkillTestTarget <- getSkillTestTarget
        for_ mSkillTestTarget $ \case
          target@(EnemyTarget eid) | effectTarget == target -> do
            engaged <- eid <=~> enemyEngagedWith iid
            unless engaged do
              withSkillTest \sid ->
                push $ skillTestModifier sid attrs (InvestigatorTarget iid) (DamageDealt 1)
          _ -> pure ()
        pure e
    SkillTestEnds _ _ _ -> do
      push $ DisableEffect effectId
      pure e
    _ -> Marksmanship1Effect <$> runMessage msg attrs
