module Arkham.Enemy.Cards.SubterraneanBeast (subterraneanBeast) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelf)
import Arkham.Matcher

newtype SubterraneanBeast = SubterraneanBeast EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

subterraneanBeast :: EnemyCard SubterraneanBeast
subterraneanBeast = enemy SubterraneanBeast Cards.subterraneanBeast (4, Static 4, 3) (1, 1)

instance HasModifiersFor SubterraneanBeast where
  getModifiersFor (SubterraneanBeast a) = do
    n <- perPlayer 3
    modifySelf a [HealthModifier n]
    when a.ready do
      modifySelect
        a
        (assetIs Assets.mineCartReliableButBroken <> AssetAt (locationWithEnemy a))
        [CannotMove]

instance HasAbilities SubterraneanBeast where
  getAbilities (SubterraneanBeast a) =
    extend1 a
      $ restricted a 1 (OnSameLocation <> thisExists a ReadyEnemy)
      $ actionAbilityWithCost (ClueCost $ PerPlayer 1)

instance RunMessage SubterraneanBeast where
  runMessage msg e@(SubterraneanBeast attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      exhaustThis attrs
      roundModifier (attrs.ability 1) attrs CannotReady
      pure e
    _ -> SubterraneanBeast <$> liftRunMessage msg attrs
