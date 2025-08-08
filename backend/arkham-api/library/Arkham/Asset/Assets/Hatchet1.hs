module Arkham.Asset.Assets.Hatchet1 (hatchet1) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest.Target
import Arkham.Matcher hiding (EnemyDefeated)
import Arkham.Matcher qualified as Matcher
import Arkham.Modifier
import Arkham.Placement

newtype Hatchet1 = Hatchet1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hatchet1 :: AssetCard Hatchet1
hatchet1 = asset Hatchet1 Cards.hatchet1

instance HasAbilities Hatchet1 where
  getAbilities (Hatchet1 a) =
    [restricted a 1 ControlsThis fightAction_]
      <> case a.placement of
        AttachedToEnemy eid ->
          [ withTooltip "Take control of Hatchet"
              $ restricted (proxied eid a) 2 OnSameLocation
              $ freeReaction
              $ Matcher.EnemyDefeated #when Anyone ByAny (be eid)
          ]
        _ -> []

instance RunMessage Hatchet1 where
  runMessage msg a@(Hatchet1 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifiers sid attrs iid [AddSkillValue #agility, DamageDealt 1]
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    EnemyDefeated _ _ (isAbilitySource attrs 1 -> True) _ -> do
      case attrs.controller of
        Just iid -> toDiscardBy iid (attrs.ability 1) attrs
        Nothing -> toDiscard (attrs.ability 1) attrs
      pure a
    SkillTestEnds _ _ (isAbilitySource attrs 1 -> True) -> do
      whenJustM getSkillTestTarget \case
        EnemyTarget eid -> do
          stillAlive <- eid <=~> AnyInPlayEnemy
          when stillAlive do
            push $ LoseControlOfAsset attrs.id
            push $ PlaceAsset attrs.id (AttachedToEnemy eid)
        _ -> pure ()
      pure a
    UseThisAbility iid (isProxySource attrs -> True) 2 -> do
      push $ TakeControlOfAsset iid attrs.id
      pure a
    _ -> Hatchet1 <$> liftRunMessage msg attrs
