module Arkham.Event.Events.TelescopicSight3 (
  telescopicSight3,
  telescopicSight3Effect,
  TelescopicSight3 (..),
) where

import Arkham.Ability
import Arkham.Effect.Import
import Arkham.Effect.Types (targetL)
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modified_, modifyEachMaybe)
import Arkham.Helpers.Window ()
import Arkham.Keyword (Keyword (Aloof, Retaliate))
import Arkham.Matcher
import Arkham.Placement
import Arkham.Taboo
import Arkham.Window qualified as Window

newtype TelescopicSight3 = TelescopicSight3 EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

telescopicSight3 :: EventCard TelescopicSight3
telescopicSight3 = event TelescopicSight3 Cards.telescopicSight3

instance HasModifiersFor TelescopicSight3 where
  getModifiersFor (TelescopicSight3 a) =
    case a.placement of
      AttachedToAsset aid _ -> do
        abilities <- select (AbilityOnAsset (AssetWithId aid) <> AbilityIsAction #fight)
        modifyEachMaybe a (map (AbilityTarget a.controller) abilities) \_ -> do
          lid <- MaybeT $ selectOne $ locationWithInvestigator a.controller
          engaged <- lift $ selectAny $ enemyEngagedWith a.controller
          let handleTaboo = if tabooed TabooList19 a then id else (<> not_ (enemyEngagedWith a.owner))
          pure
            $ if engaged && not (tabooed TabooList19 a)
              then [EnemyFightActionCriteria $ CriteriaOverride Never]
              else
                [ CanModify
                    $ EnemyFightActionCriteria
                    $ CriteriaOverride
                    $ EnemyCriteria
                    $ ThisEnemy
                    $ handleTaboo
                    $ EnemyWithoutModifier CannotBeAttacked
                    <> NonEliteEnemy
                    <> at_ (orConnected lid)
                ]
      _ -> pure mempty

instance HasAbilities TelescopicSight3 where
  getAbilities (TelescopicSight3 a) = case a.placement of
    AttachedToAsset aid _ ->
      [ restricted a 1 ControlsThis
          $ ReactionAbility
            (ActivateAbility #when (You <> UnengagedInvestigator) $ AssetAbility (AssetWithId aid) <> #fight)
            (exhaust a)
      ]
    _ -> []

instance RunMessage TelescopicSight3 where
  runMessage msg e@(TelescopicSight3 attrs) = runQueueT $ case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      assets <- select $ assetControlledBy iid <> AssetInTwoHandSlots
      chooseTargetM iid assets \asset -> push $ PlaceEvent eid $ AttachedToAsset asset Nothing
      pure e
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      createCardEffect Cards.telescopicSight3 Nothing (attrs.ability 1) iid
      pure e
    _ -> TelescopicSight3 <$> liftRunMessage msg attrs

newtype TelescopicSight3Effect = TelescopicSight3Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

telescopicSight3Effect :: EffectArgs -> TelescopicSight3Effect
telescopicSight3Effect = cardEffect TelescopicSight3Effect Cards.telescopicSight3

-- Once telescopicSight3 has been played this effect portion is a little easier,
-- we still have to replace the criteria, but since we don't have a specific
-- enemy, we add this modifier to all enemies, and in order to have it only be
-- valid during targetting, we disable it as soon as the fight enemy message is
-- processed.

-- Additionally since there are effects that touch different things, we
-- "swizzle" the target in order to disable/enable to appropriate effects

instance HasModifiersFor TelescopicSight3Effect where
  getModifiersFor (TelescopicSight3Effect a) = case a.target.investigator of
    Just iid ->
      modified_
        a
        iid
        [ EnemyFightActionCriteria
            $ CriteriaOverride
            $ EnemyCriteria
            $ ThisEnemy
            $ EnemyWithoutModifier CannotBeAttacked
            <> NonEliteEnemy
            <> at_ (orConnected $ locationWithInvestigator iid)
            <> NotEnemy (enemyEngagedWith iid)
        ]
    _ -> pure mempty

instance RunMessage TelescopicSight3Effect where
  runMessage msg e@(TelescopicSight3Effect attrs) = runQueueT $ case msg of
    FightEnemy sid iid eid _ _ _ _ -> do
      ignored <- selectAny $ EnemyWithId eid <> oneOf [EnemyWithKeyword Retaliate, EnemyWithKeyword Aloof]
      skillTestModifiers sid attrs.source iid [IgnoreRetaliate, IgnoreAloof]
      when ignored do
        checkAfter $ Window.CancelledOrIgnoredCardOrGameEffect attrs.source
      pure . TelescopicSight3Effect $ attrs & targetL .~ EnemyTarget eid
    SkillTestEnds _ _ _ -> disableReturn e
    _ -> TelescopicSight3Effect <$> liftRunMessage msg attrs
