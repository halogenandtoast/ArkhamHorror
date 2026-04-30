module Arkham.Asset.Assets.DrHenryArmitage_c2026 (drHenryArmitage_c2026) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection

newtype DrHenryArmitage_c2026 = DrHenryArmitage_c2026 AssetAttrs
  deriving anyclass (IsAsset, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drHenryArmitage_c2026 :: AssetCard DrHenryArmitage_c2026
drHenryArmitage_c2026 = ally DrHenryArmitage_c2026 Cards.drHenryArmitage_c2026 (3, 3)

instance HasModifiersFor DrHenryArmitage_c2026 where
  getModifiersFor (DrHenryArmitage_c2026 a) = for_ a.controller \iid -> do
    modified_ a iid [SkillModifier #willpower 1, SkillModifier #intellect 1]
    actions <- fieldMap InvestigatorActionsPerformed concat iid
    when (null actions) do
      modified_ a iid $ ActionDoesNotCauseAttacksOfOpportunity <$> [minBound ..]
