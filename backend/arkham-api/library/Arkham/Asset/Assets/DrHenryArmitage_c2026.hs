module Arkham.Asset.Assets.DrHenryArmitage_c2026 (drHenryArmitage_c2026) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Projection
import Arkham.Investigator.Types (Field (..))

newtype DrHenryArmitage_c2026 = DrHenryArmitage_c2026 AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drHenryArmitage_c2026 :: AssetCard DrHenryArmitage_c2026
drHenryArmitage_c2026 = ally DrHenryArmitage_c2026 Cards.drHenryArmitage_c2026 (3, 3)

instance HasModifiersFor DrHenryArmitage_c2026 where
  getModifiersFor (DrHenryArmitage_c2026 a) = case a.controller of
    Nothing -> pure mempty
    Just iid -> do
      actions <- fieldMap InvestigatorActionsPerformed concat iid
      modified_ a iid
        $ [SkillModifier #willpower 1, SkillModifier #intellect 1]
        <> (if null actions then ActionDoesNotCauseAttacksOfOpportunity <$> [minBound ..] else [])

instance RunMessage DrHenryArmitage_c2026 where
  runMessage msg (DrHenryArmitage_c2026 attrs) = runQueueT $ case msg of
    _ -> DrHenryArmitage_c2026 <$> liftRunMessage msg attrs
