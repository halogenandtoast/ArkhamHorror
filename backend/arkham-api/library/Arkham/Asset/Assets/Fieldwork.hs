module Arkham.Asset.Assets.Fieldwork (fieldwork) where

import Arkham.Ability hiding (you)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Script
import Arkham.Effect.Builder
import Arkham.Matcher
import Arkham.Modifier

newtype Fieldwork = Fieldwork AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fieldwork :: AssetCard Fieldwork
fieldwork = asset Fieldwork Cards.fieldwork

instance HasAbilities Fieldwork where
  getAbilities (Fieldwork a) = [reaction a 1 ControlsThis (exhaust a) (Enters #after You LocationWithAnyClues)]

instance RunMessage Fieldwork where
  runMessage = script $ onAbility 1 $ effect you do
    during yourNextSkillTest
    removeOn endOfCurrentPhase
    apply $ AnySkillValue 2
