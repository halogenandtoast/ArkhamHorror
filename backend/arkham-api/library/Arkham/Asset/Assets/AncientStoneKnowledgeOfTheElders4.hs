module Arkham.Asset.Assets.AncientStoneKnowledgeOfTheElders4 (ancientStoneKnowledgeOfTheElders4) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Message.Lifted.Choose

newtype AncientStoneKnowledgeOfTheElders4 = AncientStoneKnowledgeOfTheElders4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientStoneKnowledgeOfTheElders4
  :: AssetCard AncientStoneKnowledgeOfTheElders4
ancientStoneKnowledgeOfTheElders4 = asset AncientStoneKnowledgeOfTheElders4 Cards.ancientStoneKnowledgeOfTheElders4

instance HasAbilities AncientStoneKnowledgeOfTheElders4 where
  getAbilities (AncientStoneKnowledgeOfTheElders4 a) =
    [ controlled a 1 (canDamageEnemyAt (a.ability 1) YourLocation)
        $ triggered (DrawsCards #when You AnyCards AnyValue) (DynamicUseCost (be a) Secret DrawnCardsValue)
    ]

instance RunMessage AncientStoneKnowledgeOfTheElders4 where
  runMessage msg a@(AncientStoneKnowledgeOfTheElders4 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (totalUsesPayment -> damage) -> do
      chooseDamageEnemy iid (attrs.ability 1) (locationWithInvestigator iid) AnyEnemy damage
      pure a
    _ -> AncientStoneKnowledgeOfTheElders4 <$> liftRunMessage msg attrs
