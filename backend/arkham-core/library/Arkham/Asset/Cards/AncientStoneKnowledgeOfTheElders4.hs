module Arkham.Asset.Cards.AncientStoneKnowledgeOfTheElders4 (
  ancientStoneKnowledgeOfTheElders4,
  AncientStoneKnowledgeOfTheElders4 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.CampaignLogKey
import Arkham.DamageEffect
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Timing qualified as Timing

newtype AncientStoneKnowledgeOfTheElders4 = AncientStoneKnowledgeOfTheElders4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

ancientStoneKnowledgeOfTheElders4
  :: AssetCard AncientStoneKnowledgeOfTheElders4
ancientStoneKnowledgeOfTheElders4 =
  asset
    AncientStoneKnowledgeOfTheElders4
    Cards.ancientStoneKnowledgeOfTheElders4

instance HasAbilities AncientStoneKnowledgeOfTheElders4 where
  getAbilities (AncientStoneKnowledgeOfTheElders4 a) =
    [ restrictedAbility
        a
        1
        (ControlsThis <> EnemyCriteria (EnemyExists $ EnemyAt YourLocation) <> CanDealDamage)
        $ ReactionAbility
          (DrawsCards Timing.When You AnyValue)
          (DynamicUseCost (AssetWithId $ toId a) Secret DrawnCardsValue)
    ]

getDamage :: Payment -> Int
getDamage (UsesPayment n) = n
getDamage (Payments ps) = sum $ map getDamage ps
getDamage _ = 0

instance RunMessage AncientStoneKnowledgeOfTheElders4 where
  runMessage msg a@(AncientStoneKnowledgeOfTheElders4 attrs) = case msg of
    InvestigatorPlayedAsset _ aid | aid == toId attrs -> do
      n <- getRecordCount YouHaveIdentifiedTheStone
      AncientStoneKnowledgeOfTheElders4
        <$> runMessage
          msg
          (attrs {assetPrintedUses = Uses Secret (Static n), assetUses = singletonMap Secret n})
    UseCardAbility iid (isSource attrs -> True) 1 _ (getDamage -> damage) -> do
      enemies <- selectList $ EnemyAt $ locationWithInvestigator iid
      player <- getPlayer iid
      push
        $ chooseOrRunOne player
        $ [ targetLabel enemy [EnemyDamage enemy $ nonAttack attrs damage]
          | enemy <- enemies
          ]
      pure a
    _ -> AncientStoneKnowledgeOfTheElders4 <$> runMessage msg attrs
