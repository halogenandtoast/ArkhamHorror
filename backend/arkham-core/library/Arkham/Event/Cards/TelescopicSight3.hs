module Arkham.Event.Cards.TelescopicSight3 (
  telescopicSight3,
  telescopicSight3Effect,
  TelescopicSight3 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner hiding (targetL)
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Window
import Arkham.Keyword (Keyword (Aloof, Retaliate))
import Arkham.Matcher
import Arkham.Placement
import Arkham.Timing qualified as Timing
import Arkham.Window (mkWindow)
import Arkham.Window qualified as Window

newtype TelescopicSight3 = TelescopicSight3 EventAttrs
  deriving anyclass (IsEvent)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

telescopicSight3 :: EventCard TelescopicSight3
telescopicSight3 = event TelescopicSight3 Cards.telescopicSight3

instance HasModifiersFor TelescopicSight3 where
  getModifiersFor (AbilityTarget iid ability) (TelescopicSight3 a) =
    case eventPlacement a of
      AttachedToAsset aid _ | AssetSource aid == abilitySource ability -> do
        if #fight `elem` abilityActions ability
          then do
            mlid <- selectOne $ locationWithInvestigator iid
            case mlid of
              Nothing -> pure []
              Just lid -> do
                engaged <- selectAny $ enemyEngagedWith iid
                pure
                  . toModifiers a
                  $ if engaged
                    then [EnemyFightActionCriteria $ CriteriaOverride Never]
                    else
                      [ CanModify
                          $ EnemyFightActionCriteria
                          $ CriteriaOverride
                          $ EnemyCriteria
                          $ ThisEnemy
                          $ EnemyWithoutModifier CannotBeAttacked
                          <> NonEliteEnemy
                          <> EnemyAt
                            ( LocationMatchAny
                                [ LocationWithId lid
                                , ConnectedTo $ LocationWithId lid
                                ]
                            )
                          <> NotEnemy (enemyEngagedWith $ eventOwner a)
                      ]
          else pure []
      _ -> pure []
  getModifiersFor _ _ = pure []

instance HasAbilities TelescopicSight3 where
  getAbilities (TelescopicSight3 a) = case eventPlacement a of
    AttachedToAsset aid _ ->
      [ restrictedAbility a 1 ControlsThis
          $ ReactionAbility
            ( ActivateAbility Timing.When You
                $ AssetAbility (AssetWithId aid)
                <> AbilityIsAction Action.Fight
            )
            (ExhaustCost (toTarget a))
      ]
    _ -> []

instance RunMessage TelescopicSight3 where
  runMessage msg e@(TelescopicSight3 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      assets <- selectList $ assetControlledBy iid <> AssetInTwoHandSlots
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ targetLabel asset [PlaceEvent iid eid $ AttachedToAsset asset Nothing]
          | asset <- assets
          ]
      pure e
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      pushAll
        [ createCardEffect
            Cards.telescopicSight3
            Nothing
            (toSource attrs)
            (InvestigatorTarget iid)
        ]
      pure e
    _ -> TelescopicSight3 <$> runMessage msg attrs

newtype TelescopicSight3Effect = TelescopicSight3Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

telescopicSight3Effect :: EffectArgs -> TelescopicSight3Effect
telescopicSight3Effect =
  cardEffect TelescopicSight3Effect Cards.telescopicSight3

-- Once telescopicSight3 has been played this effect portion is a little easier,
-- we still have to replace the criteria, but since we don't have a specific
-- enemy, we add this modifier to all enemies, and in order to have it only be
-- valid during targetting, we disable it as soon as the fight enemy message is
-- processed.

-- Additionally since there are effects that touch different things, we
-- "swizzle" the target in order to disable/enable to appropriate effects

instance HasModifiersFor TelescopicSight3Effect where
  getModifiersFor (InvestigatorTarget iid) (TelescopicSight3Effect a) =
    case effectTarget a of
      InvestigatorTarget iid'
        | iid == iid' ->
            pure
              $ toModifiers
                a
                [ EnemyFightActionCriteria
                    $ CriteriaOverride
                    $ EnemyCriteria
                      ( ThisEnemy
                          $ EnemyWithoutModifier CannotBeAttacked
                          <> NonEliteEnemy
                          <> EnemyAt
                            ( LocationMatchAny
                                [ locationWithInvestigator iid
                                , ConnectedTo $ locationWithInvestigator iid
                                ]
                            )
                          <> NotEnemy (enemyEngagedWith iid)
                      )
                ]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage TelescopicSight3Effect where
  runMessage msg e@(TelescopicSight3Effect attrs@EffectAttrs {..}) =
    case msg of
      FightEnemy iid eid _ _ _ _ -> do
        ignored <-
          selectAny
            $ EnemyWithId eid
            <> EnemyOneOf
              [EnemyWithKeyword Retaliate, EnemyWithKeyword Aloof]
        ignoreWindow <-
          checkWindows
            [ mkWindow
                Timing.After
                (Window.CancelledOrIgnoredCardOrGameEffect effectSource)
            ]
        pushAll
          $ skillTestModifiers
            (toSource attrs)
            (InvestigatorTarget iid)
            [IgnoreRetaliate, IgnoreAloof]
          : [ignoreWindow | ignored]
        pure . TelescopicSight3Effect $ attrs & targetL .~ EnemyTarget eid
      SkillTestEnds _ _ -> do
        push $ DisableEffect effectId
        pure e
      _ -> TelescopicSight3Effect <$> runMessage msg attrs
