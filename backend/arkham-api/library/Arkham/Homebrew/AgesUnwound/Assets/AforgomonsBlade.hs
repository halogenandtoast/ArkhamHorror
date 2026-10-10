module Arkham.Homebrew.AgesUnwound.Assets.AforgomonsBlade (aforgomonsBlade) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Homebrew.AgesUnwound.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.AgesUnwound.Helpers (campaignI18n)
import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype AforgomonsBlade = AforgomonsBlade AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | "Uses (3 charges)." — the charges are on the def.
aforgomonsBlade :: AssetCard AforgomonsBlade
aforgomonsBlade = asset AforgomonsBlade Cards.aforgomonsBlade

{- | "[action]: Fight. You get +1 [combat] and deal +1 damage for this attack. If
you succeed, you may spend 1 charge and exhaust Aforgomon's Blade to gain 1
action and give the attacked enemy -1 fight and -1 evade until the end of the
round."

The charge and the exhaust are the /rider's/ cost, not the attack's, so the
ability itself costs nothing but the action.
-}
instance HasAbilities AforgomonsBlade where
  getAbilities (AforgomonsBlade a) =
    [restricted a 1 ControlsThis $ fightAction mempty]

instance RunMessage AforgomonsBlade where
  runMessage msg a@(AforgomonsBlade attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifiers sid (attrs.ability 1) iid [SkillModifier #combat 1, DamageDealt 1]
      chooseFightEnemyEdit sid iid (attrs.ability 1) (setTarget attrs)
      pure a
    Successful (Action.Fight, EnemyTarget eid) iid _ (isTarget attrs -> True) _ -> do
      when (hasUses attrs && not attrs.exhausted) do
        chooseOneM iid $ campaignI18n $ scope "aforgomonsBlade" do
          labeled "spendChargeForAction" do
            push $ SpendUses (attrs.ability 1) (toTarget attrs) Charge 1
            exhaustThis attrs
            gainActions iid (attrs.ability 1) 1
            roundModifiers (attrs.ability 1) eid [EnemyFight (-1), EnemyEvade (-1)]
          labeled "doNotSpendCharge" nothing
      pure a
    _ -> AforgomonsBlade <$> liftRunMessage msg attrs
