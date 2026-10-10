module Arkham.Event.Events.TelescopicSight3 (telescopicSight3, telescopicSight3Effect) where

import Arkham.Ability
import Arkham.Classes.HasGame (HasGame)
import Arkham.Effect.Import
import Arkham.Effect.Types (targetL)
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted hiding (choose, targetL)
import Arkham.Helpers.CombatTarget (getAttackRangeBonus)
import Arkham.Helpers.Modifiers (ModifierType (..), modified_, modifyEachMaybe)
import Arkham.Helpers.Window ()
import Arkham.Keyword (Keyword (Aloof, Retaliate))
import Arkham.Matcher
import Arkham.Message.Lifted.Upgrade
import Arkham.Taboo
import Arkham.Window qualified as Window

newtype TelescopicSight3 = TelescopicSight3 EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

telescopicSight3 :: EventCard TelescopicSight3
telescopicSight3 = event TelescopicSight3 Cards.telescopicSight3

{- | How far an attack with the attached asset reaches. This card sets the standard
range to a connecting location, and Springfield M1903's taboo adds a location to
whatever that range is, so the two stack.
-}
attackRange :: HasGame m => AssetId -> m Int
attackRange aid = (1 +) <$> getAttackRangeBonus aid

instance HasModifiersFor TelescopicSight3 where
  getModifiersFor (TelescopicSight3 a) =
    case a.placement of
      AttachedToAsset aid _ -> do
        range <- attackRange aid
        abilities <- select (AbilityOnAsset (AssetWithId aid) <> AbilityIsAction #fight)
        modifyEachMaybe a (map (AbilityTarget a.controller . abilityToRef) abilities) \_ -> do
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
                    <> at_ (withinDistance range lid)
                ]
      _ -> pure mempty

instance HasAbilities TelescopicSight3 where
  getAbilities (TelescopicSight3 a) = case a.placement of
    AttachedToAsset aid _ ->
      [ restricted a 1 ControlsThis
          $ triggered
            (ActivateAbility #when (You <> UnengagedInvestigator) $ AssetAbility (AssetWithId aid) <> #fight)
            (exhaust a)
      ]
    _ -> []

instance RunMessage TelescopicSight3 where
  runMessage msg e@(TelescopicSight3 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      assets <- getUpgradeTargets iid $ assetControlledBy iid <> AssetInTwoHandSlots
      chooseTargetM iid assets \asset -> place attrs $ AttachedToAsset asset Nothing
      pure e
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      range <- case attrs.placement of
        AttachedToAsset aid _ -> attackRange aid
        _ -> pure 1
      createCardEffect Cards.telescopicSight3 (effectInt range) (attrs.ability 1) iid
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
    Just iid -> do
      let range = fromMaybe 1 ((.int) =<< a.metadata)
      modified_
        a
        iid
        [ AttackRangeIncrease 1
        , EnemyFightActionCriteria
            $ CriteriaOverride
            $ EnemyCriteria
            $ ThisEnemy
            $ EnemyWithoutModifier CannotBeAttacked
            <> NonEliteEnemy
            <> at_ (withinDistance range $ locationWithInvestigator iid)
            <> NotEnemy (enemyEngagedWith iid)
        ]
    _ -> pure mempty

instance RunMessage TelescopicSight3Effect where
  runMessage msg e@(TelescopicSight3Effect attrs) = runQueueT $ case msg of
    FightEnemy eid choose -> do
      let sid = choose.skillTest
      let iid = choose.investigator
      ignored <- selectAny $ EnemyWithId eid <> oneOf [EnemyWithKeyword Retaliate, EnemyWithKeyword Aloof]
      skillTestModifiers sid attrs.source iid [IgnoreRetaliate, IgnoreAloof]
      when ignored do
        checkAfter $ Window.CancelledOrIgnoredCardOrGameEffect attrs.source Nothing
      pure . TelescopicSight3Effect $ attrs & targetL .~ EnemyTarget eid
    SkillTestEnds _ _ _ -> disableReturn e
    _ -> TelescopicSight3Effect <$> liftRunMessage msg attrs
