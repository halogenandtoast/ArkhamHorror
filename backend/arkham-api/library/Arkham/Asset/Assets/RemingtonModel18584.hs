module Arkham.Asset.Assets.RemingtonModel18584 (remingtonModel18584) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Matcher
import Arkham.Modifier

newtype RemingtonModel18584 = RemingtonModel18584 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

remingtonModel18584 :: AssetCard RemingtonModel18584
remingtonModel18584 = asset RemingtonModel18584 Cards.remingtonModel18584

instance HasAbilities RemingtonModel18584 where
  getAbilities (RemingtonModel18584 a) =
    [ fightAbility a 1 (assetUseCost a Ammo 1) ControlsThis
    , controlled a 2 (exists $ CanFightEnemy (toSource a))
        $ freeReaction (oneOf [AssetLeavesPlay #when (be a), AssetEntersPlay #when (be a)])
    ]

doAbility1 :: ReverseQueue m => InvestigatorId -> AssetAttrs -> m ()
doAbility1 iid attrs = do
  sid <- getRandom
  skillTestModifiers sid (attrs.ability 1) iid [SkillModifier #combat 2, DamageDealt 1]
  chooseFightEnemy sid iid (attrs.ability 1)

instance RunMessage RemingtonModel18584 where
  runMessage msg a@(RemingtonModel18584 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) n | n `elem` [1, 2] -> do
      doAbility1 iid attrs
      pure a
    _ -> RemingtonModel18584 <$> liftRunMessage msg attrs
