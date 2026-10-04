module Arkham.Homebrew.AgainstTheWendigo.Assets.Tomahawk (tomahawk) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Assets qualified as Cards

newtype Tomahawk = Tomahawk AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tomahawk :: AssetCard Tomahawk
tomahawk = asset Tomahawk Cards.tomahawk

instance HasAbilities Tomahawk where
  getAbilities (Tomahawk a) =
    [ restricted a 1 ControlsThis $ fightAction_
    , restricted a 2 ControlsThis $ fightAction (DiscardCost FromPlay $ toTarget a)
    ]

instance RunMessage Tomahawk where
  runMessage msg a@(Tomahawk attrs) = runQueueT $ case msg of
    -- "Fight. You get +1 [combat] for this attack. This attack deals +1 damage."
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifiers sid (attrs.ability 1) iid [SkillModifier #combat 1, DamageDealt 1]
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    -- "Discard Tomahawk: Fight. Use [agility] instead of [combat] for this
    -- attack. This attack deals +2 damage."
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      skillTestModifiers
        sid
        (attrs.ability 2)
        iid
        [BaseSkillOf #combat 0, UseSkillInsteadOf #combat #agility, DamageDealt 2]
      chooseFightEnemy sid iid (attrs.ability 2)
      pure a
    _ -> Tomahawk <$> liftRunMessage msg attrs
