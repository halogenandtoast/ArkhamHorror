module Arkham.Asset.Cards.AncientStoneTransientThoughts4 (ancientStoneTransientThoughts4, AncientStoneTransientThoughts4 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.CampaignLogKey
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Movement
import Arkham.Prelude

newtype AncientStoneTransientThoughts4 = AncientStoneTransientThoughts4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

ancientStoneTransientThoughts4
  :: AssetCard AncientStoneTransientThoughts4
ancientStoneTransientThoughts4 = asset AncientStoneTransientThoughts4 Cards.ancientStoneTransientThoughts4

instance HasAbilities AncientStoneTransientThoughts4 where
  getAbilities (AncientStoneTransientThoughts4 a) =
    [ controlledAbility a 1 (exists $ You <> InvestigatorCanMove)
        $ ReactionAbility
          (DrawsCards #when You AnyValue)
          (DynamicUseCost (AssetWithId $ toId a) Secret DrawnCardsValue)
    ]

getMoves :: Payment -> Int
getMoves (UsesPayment n) = n
getMoves (Payments ps) = sum $ map getMoves ps
getMoves _ = 0

instance RunMessage AncientStoneTransientThoughts4 where
  runMessage msg a@(AncientStoneTransientThoughts4 attrs) = case msg of
    InvestigatorPlayedAsset _ aid | aid == toId attrs -> do
      n <- getRecordCount YouHaveIdentifiedTheStone
      AncientStoneTransientThoughts4
        <$> runMessage
          msg
          (attrs {assetPrintedUses = Uses Secret (Static n), assetUses = singletonMap Secret n})
    UseCardAbility iid (isSource attrs -> True) 1 ws p@(getMoves -> n) -> do
      pushAll $ replicate n $ UseCardAbilityStep iid (toSource attrs) 1 ws p 1
      pure a
    UseCardAbilityStep iid (isSource attrs -> True) 1 _ _ 1 -> do
      targets <- selectList $ AccessibleFrom $ locationWithInvestigator iid
      player <- getPlayer iid
      pushWhen (notNull targets)
        $ chooseOne player
        $ targetLabels targets (only . Move . move (toAbilitySource attrs 1) iid)
      pure a
    _ -> AncientStoneTransientThoughts4 <$> runMessage msg attrs
