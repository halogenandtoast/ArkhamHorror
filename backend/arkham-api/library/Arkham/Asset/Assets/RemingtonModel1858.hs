module Arkham.Asset.Assets.RemingtonModel1858 (remingtonModel1858) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Matcher
import Arkham.Modifier

newtype RemingtonModel1858 = RemingtonModel1858 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

remingtonModel1858 :: AssetCard RemingtonModel1858
remingtonModel1858 = asset RemingtonModel1858 Cards.remingtonModel1858

instance HasAbilities RemingtonModel1858 where
  getAbilities (RemingtonModel1858 a) =
    [ fightAbility a 1 (assetUseCost a Ammo 1) ControlsThis
    , controlled a 2 (exists $ CanFightEnemy (toSource a)) $ freeReaction (AssetLeavesPlay #when (be a))
    ]

doAbility1 :: ReverseQueue m => InvestigatorId -> AssetAttrs -> m ()
doAbility1 iid attrs = do
  sid <- getRandom
  skillTestModifiers sid (attrs.ability 1) iid [SkillModifier #combat 1, DamageDealt 1]
  chooseFightEnemy sid iid (attrs.ability 1)

instance RunMessage RemingtonModel1858 where
  runMessage msg a@(RemingtonModel1858 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) n | n `elem` [1, 2] -> do
      doAbility1 iid attrs
      pure a
    _ -> RemingtonModel1858 <$> liftRunMessage msg attrs
