module Arkham.Asset.Assets.DeloresGadling (
  deloresGadling,
  DeloresGadling(..),
) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Trait

newtype DeloresGadling = DeloresGadling AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deloresGadling :: AssetCard DeloresGadling
deloresGadling = allyWith DeloresGadling Cards.deloresGadling (1, 3) noSlots

instance HasModifiersFor DeloresGadling where
  getModifiersFor (DeloresGadling a) = for_ a.controller \iid -> \case
    InvestigatorTarget iid' | iid == iid' -> do
      action <- getSkillTestAction
      n <- selectCount $ EnemyAt YourLocation <> EnemyWithTrait Humanoid
      pure $ [SkillModifier #intellect n | n > 0 && action `elem` [Just #investigate, Just Parley]]
        <> [MayIgnoreAttacksOfOpportunity]
    _ -> pure mempty

instance RunMessage DeloresGadling where
  runMessage msg a@(DeloresGadling attrs) = runQueueT $ case msg of
    _ -> DeloresGadling <$> liftRunMessage msg attrs
