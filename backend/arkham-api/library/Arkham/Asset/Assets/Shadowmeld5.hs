module Arkham.Asset.Assets.Shadowmeld5 (shadowmeld5) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Evade
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Modifier

newtype Shadowmeld5 = Shadowmeld5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shadowmeld5 :: AssetCard Shadowmeld5
shadowmeld5 = asset Shadowmeld5 Cards.shadowmeld5

instance HasAbilities Shadowmeld5 where
  getAbilities (Shadowmeld5 a) = [controlled_ a 1 $ evadeActionWith_ #willpower]

instance RunMessage Shadowmeld5 where
  runMessage msg a@(Shadowmeld5 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      onRevealChaosTokenEffect sid #tablet attrs attrs $ doStep 1 msg
      skillTestModifier sid source iid (SkillModifier #willpower 2)
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
        -- I expect this to be errata'd
        -- locations <- select $ LocationWithDistanceFromAtMost 2 (locationWithInvestigator iid) RevealedLocation
        locations <- select $ LocationWithDistanceFromAtMost 2 (locationWithInvestigator iid) Anywhere
        unless (null locations) do
          chooseOrRunOneM iid do
            labeled "Do not move" nothing
            targets locations \lid -> do
              removeTokens (attrs.ability 1) attrs Charge 1
              moveTo attrs iid lid
      pure a
    _ -> Shadowmeld5 <$> liftRunMessage msg attrs
