module Arkham.Asset.Assets.DrMyaBadry (drMyaBadry) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Scenarios.TheMidwinterGala.Helpers

newtype DrMyaBadry = DrMyaBadry AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drMyaBadry :: AssetCard DrMyaBadry
drMyaBadry = allyWith DrMyaBadry Cards.drMyaBadry (2, 2) noSlots

instance HasModifiersFor DrMyaBadry where
  getModifiersFor (DrMyaBadry a) = do
    controllerGets a [HandSize 2]
    handleSpellbound a

instance HasAbilities DrMyaBadry where
  getAbilities (DrMyaBadry a) = [investigateAbility a 1 (exhaust a) ControlsThis]

instance RunMessage DrMyaBadry where
  runMessage msg a@(DrMyaBadry attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      cardsInHand <- fieldMap InvestigatorHand length iid
      sid <- getRandom
      skillTestModifier sid (attrs.ability 1) iid (BaseSkillOf #intellect cardsInHand)
      investigate sid iid (attrs.ability 1)
      pure a
    Flip _ ScenarioSource (isTarget attrs -> True) -> do
      pure $ DrMyaBadry $ attrs & flippedL .~ True & visibleL .~ False & setMeta True
    Flip _ _ (isTarget attrs -> True) -> do
      let flipped = not $ view flippedL attrs
      pure $ DrMyaBadry $ attrs & flippedL .~ flipped & visibleL .~ True & setMeta False
    _ -> DrMyaBadry <$> liftRunMessage msg attrs
