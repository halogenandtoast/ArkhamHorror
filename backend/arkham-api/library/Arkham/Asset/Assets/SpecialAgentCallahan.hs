module Arkham.Asset.Assets.SpecialAgentCallahan (
  specialAgentCallahan,
  SpecialAgentCallahan(..),
) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher

newtype SpecialAgentCallahan = SpecialAgentCallahan AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

specialAgentCallahan :: AssetCard SpecialAgentCallahan
specialAgentCallahan = allyWith SpecialAgentCallahan Cards.specialAgentCallahan (3, 1) noSlots

instance HasAbilities SpecialAgentCallahan where
  getAbilities (SpecialAgentCallahan a) =
    [ restricted a 1 ControlsThis fightAction_
    , controlled a 2 ControlsThis
        $ ReactionAbility
            ( ActivateAbility #after You
                $ AssetAbility (hasAnyTrait [Ally, Weapon]) <> AbilityIsActionAbility
            )
            (exhaust a)
    ]

instance RunMessage SpecialAgentCallahan where
  runMessage msg a@(SpecialAgentCallahan attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifier sid (attrs.ability 1) iid (Difficulty (-1))
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      enemies <- select $ enemyAtLocationWith iid
      chooseTargetM iid enemies $ \enemy -> do
        nonAttackEnemyDamage (Just iid) (attrs.ability 2) 1 enemy
      pure a
    _ -> SpecialAgentCallahan <$> liftRunMessage msg attrs
