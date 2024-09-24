module Arkham.Asset.Cards.ElinaHarperKnowsTooMuch (
  elinaHarperKnowsTooMuch,
  ElinaHarperKnowsTooMuch (..),
)
where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Modifier

newtype ElinaHarperKnowsTooMuch = ElinaHarperKnowsTooMuch AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

elinaHarperKnowsTooMuch :: AssetCard ElinaHarperKnowsTooMuch
elinaHarperKnowsTooMuch = asset ElinaHarperKnowsTooMuch Cards.elinaHarperKnowsTooMuch

instance HasModifiersFor ElinaHarperKnowsTooMuch where
  getModifiersFor target (ElinaHarperKnowsTooMuch a) = modified a $ case target of
    InvestigatorTarget iid | iid `controls` a -> do
      actions <- fieldMap InvestigatorActionsPerformed concat iid
      pure
        $ [SkillModifier #intellect 1, SkillModifier #agility 1]
        <> (if null actions then ActionDoesNotCauseAttacksOfOpportunity <$> [minBound ..] else [])
    _ -> []

instance RunMessage ElinaHarperKnowsTooMuch where
  runMessage msg (ElinaHarperKnowsTooMuch attrs) = runQueueT $ case msg of
    _ -> ElinaHarperKnowsTooMuch <$> liftRunMessage msg attrs
