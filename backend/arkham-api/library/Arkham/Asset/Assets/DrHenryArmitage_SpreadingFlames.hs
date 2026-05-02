{- HLINT ignore "Use camelCase" -}
module Arkham.Asset.Assets.DrHenryArmitage_SpreadingFlames (drHenryArmitage_SpreadingFlames) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection

newtype DrHenryArmitage_SpreadingFlames = DrHenryArmitage_SpreadingFlames AssetAttrs
  deriving anyclass (IsAsset, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drHenryArmitage_SpreadingFlames :: AssetCard DrHenryArmitage_SpreadingFlames
drHenryArmitage_SpreadingFlames = ally DrHenryArmitage_SpreadingFlames Cards.drHenryArmitage_SpreadingFlames (3, 3)

instance HasModifiersFor DrHenryArmitage_SpreadingFlames where
  getModifiersFor (DrHenryArmitage_SpreadingFlames a) = for_ a.controller \iid -> do
    modified_ a iid [SkillModifier #willpower 1, SkillModifier #intellect 1]
    actions <- fieldMap InvestigatorActionsPerformed concat iid
    when (null actions) do
      modified_ a iid $ ActionDoesNotCauseAttacksOfOpportunity <$> [minBound ..]
