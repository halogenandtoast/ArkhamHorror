module Arkham.Asset.Assets.Shadowmeld (shadowmeld) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Evade
import Arkham.Helpers.Location (getAccessibleLocations)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype Shadowmeld = Shadowmeld AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shadowmeld :: AssetCard Shadowmeld
shadowmeld = asset Shadowmeld Cards.shadowmeld

instance HasAbilities Shadowmeld where
  getAbilities (Shadowmeld a) = [controlled_ a 1 $ evadeActionWith_ #willpower]

instance RunMessage Shadowmeld where
  runMessage msg a@(Shadowmeld attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      onRevealChaosTokenEffect sid #tablet attrs attrs $ doStep 1 msg
      chooseEvadeEnemyEdit sid iid source $ setTarget attrs . withSkillType #willpower
      pure a
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      if attrs.use Charge == 0
        then do
          loseActions iid (attrs.ability 1) 1
          toDiscardBy iid (attrs.ability 1) attrs
        else removeTokens (attrs.ability 1) attrs Charge 1
      pure a
    Successful (Action.Evade, EnemyTarget eid) iid _ target _ | isTarget attrs target -> do
      push $ EnemyEvaded iid eid
      when (attrs.use Charge > 0) do
        locations <- getAccessibleLocations iid attrs
        unless (null locations) do
          chooseOrRunOneM iid do
            labeled "Do not move to a connecting location" nothing
            targets locations \lid -> do
              removeTokens (attrs.ability 1) attrs Charge 1
              moveTo attrs iid lid
      pure a
    _ -> Shadowmeld <$> liftRunMessage msg attrs
