module Arkham.Asset.Assets.MeteoriteSample (meteoriteSample) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Enemy.Types (Field (EnemyLocation))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement (place)
import Arkham.Placement
import Arkham.Projection

newtype MeteoriteSample = MeteoriteSample AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

meteoriteSample :: AssetCard MeteoriteSample
meteoriteSample = asset MeteoriteSample Cards.meteoriteSample

instance HasAbilities MeteoriteSample where
  getAbilities (MeteoriteSample a) = case a.placement of
    AttachedToLocation lid ->
      [restricted a 1 (exists $ LocationWithId lid <> LocationWithoutClues) actionAbility]
    AtLocation lid ->
      [ restricted a 1 (Uncontrolled <> exists (LocationWithId lid <> LocationWithoutClues)) actionAbility
      ]
    _ -> []

instance RunMessage MeteoriteSample where
  runMessage msg a@(MeteoriteSample attrs) = runQueueT $ case msg of
    PlaceAsset aid (AtLocation lid) | aid == attrs.id -> do
      pure $ MeteoriteSample $ attrs {assetPlacement = AttachedToLocation lid}
    -- When the host enemy (Mi-Go Harvester) leaves play, the sample falls to
    -- its location rather than being discarded with the enemy.
    Discard _ _ (isTarget attrs -> True) | AttachedToEnemy eid <- attrs.placement -> do
      field EnemyLocation eid >>= \case
        Just lid -> place attrs (AtLocation lid) >> pure a
        Nothing -> MeteoriteSample <$> liftRunMessage msg attrs
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseBeginSkillTest sid iid (attrs.ability 1) attrs [#willpower, #agility] (Fixed 6)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      takeControlOfAsset iid attrs
      pure a
    _ -> MeteoriteSample <$> liftRunMessage msg attrs
