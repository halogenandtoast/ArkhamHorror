module Arkham.Asset.Assets.ElinaHarperKnowsTooMuch (
  elinaHarperKnowsTooMuch,
  ElinaHarperKnowsTooMuch (..),
)
where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection

newtype ElinaHarperKnowsTooMuch = ElinaHarperKnowsTooMuch AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

elinaHarperKnowsTooMuch :: AssetCard ElinaHarperKnowsTooMuch
elinaHarperKnowsTooMuch = asset ElinaHarperKnowsTooMuch Cards.elinaHarperKnowsTooMuch

instance HasModifiersFor ElinaHarperKnowsTooMuch where
  getModifiersFor (ElinaHarperKnowsTooMuch a) = case a.controller of
    Nothing -> pure mempty
    Just iid -> do
      actions <- fieldMap InvestigatorActionsPerformed concat iid
      modified_ a iid
        $ [SkillModifier #intellect 1, SkillModifier #agility 1]
        <> (if null actions then ActionDoesNotCauseAttacksOfOpportunity <$> [minBound ..] else [])

instance RunMessage ElinaHarperKnowsTooMuch where
  runMessage msg (ElinaHarperKnowsTooMuch attrs) = runQueueT $ case msg of
    _ -> ElinaHarperKnowsTooMuch <$> liftRunMessage msg attrs
