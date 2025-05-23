module Arkham.Asset.Assets.Blackjack (blackjack) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Modifier

newtype Blackjack = Blackjack AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blackjack :: AssetCard Blackjack
blackjack = asset Blackjack Cards.blackjack

instance HasAbilities Blackjack where
  getAbilities (Blackjack a) = [fightAbility a 1 mempty ControlsThis]

instance RunMessage Blackjack where
  runMessage msg a@(Blackjack attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      skillTestModifiers sid source iid [SkillModifier #combat 1, DoesNotDamageOtherInvestigator]
      chooseFightEnemy sid iid source
      pure a
    _ -> Blackjack <$> liftRunMessage msg attrs
