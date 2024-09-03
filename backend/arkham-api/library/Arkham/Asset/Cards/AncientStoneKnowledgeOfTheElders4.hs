module Arkham.Asset.Cards.AncientStoneKnowledgeOfTheElders4 (
  ancientStoneKnowledgeOfTheElders4,
  AncientStoneKnowledgeOfTheElders4 (..),
) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.DamageEffect
import Arkham.Matcher hiding (NonAttackDamageEffect)

newtype AncientStoneKnowledgeOfTheElders4 = AncientStoneKnowledgeOfTheElders4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientStoneKnowledgeOfTheElders4
  :: AssetCard AncientStoneKnowledgeOfTheElders4
ancientStoneKnowledgeOfTheElders4 = asset AncientStoneKnowledgeOfTheElders4 Cards.ancientStoneKnowledgeOfTheElders4

instance HasAbilities AncientStoneKnowledgeOfTheElders4 where
  getAbilities (AncientStoneKnowledgeOfTheElders4 a) =
    [ controlledAbility a 1 (exists (EnemyAt YourLocation) <> CanDealDamage)
        $ ReactionAbility (DrawsCards #when You AnyValue) (DynamicUseCost (be a) Secret DrawnCardsValue)
    ]

instance RunMessage AncientStoneKnowledgeOfTheElders4 where
  runMessage msg a@(AncientStoneKnowledgeOfTheElders4 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (totalUsesPayment -> damage) -> do
      enemies <- select $ EnemyAt $ locationWithInvestigator iid
      chooseOrRunOne iid [targetLabel x [EnemyDamage x $ nonAttack attrs damage] | x <- enemies]
      pure a
    _ -> AncientStoneKnowledgeOfTheElders4 <$> liftRunMessage msg attrs
