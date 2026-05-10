module Arkham.Asset.Assets.WinchesterModel522 (winchesterModel522, winchesterModel522Effect) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Effect.Import
import Arkham.Helpers.SkillTest (getSkillTestTargetedEnemy, withSkillTest)
import Arkham.I18n
import Arkham.Modifier

newtype WinchesterModel522 = WinchesterModel522 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

winchesterModel522 :: AssetCard WinchesterModel522
winchesterModel522 = asset WinchesterModel522 Cards.winchesterModel522

instance HasAbilities WinchesterModel522 where
  getAbilities (WinchesterModel522 a) =
    [ skillTestAbility
        $ (cardI18n $ withI18nTooltip "winchesterModel522.actionSpend1Ammo")
        $ controlled_ a 1
        $ fightAction (assetUseCost a Ammo 1)
    , skillTestAbility
        $ (cardI18n $ withI18nTooltip "winchesterModel522.actionDiscardWinchester")
        $ controlled_ a 2
        $ fightAction (discardCost a)
    ]

instance RunMessage WinchesterModel522 where
  runMessage msg a@(WinchesterModel522 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifiers sid (attrs.ability 1) iid [SkillModifier #combat 3, DamageDealt 1]
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      skillTestModifier sid (attrs.ability 2) iid (SkillModifier #combat 3)
      createCardEffect Cards.winchesterModel522 Nothing (attrs.ability 2) (SkillTestTarget sid)
      chooseFightEnemy sid iid (attrs.ability 2)
      pure a
    _ -> WinchesterModel522 <$> liftRunMessage msg attrs

newtype WinchesterModel522Effect = WinchesterModel522Effect EffectAttrs
  deriving anyclass (HasAbilities, HasModifiersFor, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

winchesterModel522Effect :: EffectArgs -> WinchesterModel522Effect
winchesterModel522Effect = cardEffect WinchesterModel522Effect Cards.winchesterModel522

instance RunMessage WinchesterModel522Effect where
  runMessage msg e@(WinchesterModel522Effect attrs) = runQueueT $ case msg of
    PassedSkillTest iid (Just Action.Fight) _ _ _ _ -> do
      withSkillTest \sid -> do
        when (isTarget sid attrs.target) do
          mEnemy <- getSkillTestTargetedEnemy
          for_ mEnemy \eid -> automaticallyEvadeEnemy iid eid
          disable attrs
      pure e
    SkillTestEnds sid _ _ | isTarget sid attrs.target -> disableReturn e
    _ -> WinchesterModel522Effect <$> liftRunMessage msg attrs
