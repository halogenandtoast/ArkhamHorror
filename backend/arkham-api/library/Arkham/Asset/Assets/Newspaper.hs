module Arkham.Asset.Assets.Newspaper (newspaper) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection

newtype Newspaper = Newspaper AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

newspaper :: AssetCard Newspaper
newspaper = asset Newspaper Cards.newspaper

instance HasModifiersFor Newspaper where
  getModifiersFor (Newspaper a) = for_ a.controller \iid -> do
    clueCount <- field InvestigatorClues iid
    modifiedWhen_ a (clueCount == 0) iid [ActionSkillModifier #investigate #intellect 2]

instance RunMessage Newspaper where
  runMessage msg (Newspaper attrs) = Newspaper <$> runMessage msg attrs
