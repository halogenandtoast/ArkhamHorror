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
    [ controlled_ a 1 fightAction_
    , controlled_ a 2 $ fightActionWith #agility (DiscardCost FromPlay $ toTarget a)
    ]

instance RunMessage Tomahawk where
  runMessage msg a@(Tomahawk attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifiers sid (attrs.ability 1) iid [SkillModifier #combat 1, DamageDealt 1]
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      skillTestModifiers sid (attrs.ability 2) iid [DamageDealt 2]
      chooseFightEnemyWith #agility sid iid (attrs.ability 2)
      pure a
    _ -> Tomahawk <$> liftRunMessage msg attrs
