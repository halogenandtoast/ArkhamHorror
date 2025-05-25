module Arkham.Asset.Assets.ArchaicGlyphsProphecyForetold3 (archaicGlyphsProphecyForetold3) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.SkillTest.Lifted (investigateEdit_)
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Message.Lifted.Choose

newtype ArchaicGlyphsProphecyForetold3 = ArchaicGlyphsProphecyForetold3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasAbilities ArchaicGlyphsProphecyForetold3 where
  getAbilities (ArchaicGlyphsProphecyForetold3 a) =
    [noAOO $ investigateAbility a 1 (assetUseCost a Charge 1) ControlsThis]

archaicGlyphsProphecyForetold3 :: AssetCard ArchaicGlyphsProphecyForetold3
archaicGlyphsProphecyForetold3 = asset ArchaicGlyphsProphecyForetold3 Cards.archaicGlyphsProphecyForetold3

instance RunMessage ArchaicGlyphsProphecyForetold3 where
  runMessage msg a@(ArchaicGlyphsProphecyForetold3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      investigateEdit_ sid iid (attrs.ability 1) (setTarget attrs)
      pure a
    Successful (Action.Investigate, LocationTarget lid) iid _ (isTarget attrs -> True) _ -> do
      discoverAt IsInvestigate iid (attrs.ability 1) lid 1

      enemies <- select $ enemyEngagedWith iid
      unless (null enemies) do
        chooseOneM iid do
          labeled "No evasion" nothing
          targets enemies (automaticallyEvadeEnemy iid)
      pure a
    _ -> ArchaicGlyphsProphecyForetold3 <$> liftRunMessage msg attrs
