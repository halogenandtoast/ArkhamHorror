module Arkham.Treachery.Cards.EyesInTheTrees (eyesInTheTrees, EyesInTheTrees (..)) where

import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Investigator.Types (Field (InvestigatorPlacement))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype EyesInTheTrees = EyesInTheTrees TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eyesInTheTrees :: TreacheryCard EyesInTheTrees
eyesInTheTrees = treachery EyesInTheTrees Cards.eyesInTheTrees

instance RunMessage EyesInTheTrees where
  runMessage msg t@(EyesInTheTrees attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 4)
      pure t
    FailedSkillTest iid _ (isSource attrs -> True) target@SkillTestInitiatorTarget {} sType n -> do
      field InvestigatorPlacement iid >>= \case
        InVehicle aid -> do
          selectEach (InVehicleMatching $ AssetWithId aid) \iid' ->
            doStep n (FailedSkillTest iid' Nothing (toSource attrs) target sType n)
        _ -> doStep n msg
      pure t
    DoStep n (FailedThisSkillTest iid (isSource attrs -> True)) | n > 0 -> do
      assets <- select $ assetControlledBy iid <> DiscardableAsset
      cards <- selectAny $ inHandOf iid <> basic DiscardableCard

      chooseOneM iid do
        when cards do
          labeled ("Choose and discard " <> pluralize n "card") $ chooseAndDiscardCards iid attrs n
        when (notNull assets) do
          labeled "Discard an asset you control" $ chooseTargetM iid assets $ toDiscardBy iid attrs
      pure t
    _ -> EyesInTheTrees <$> liftRunMessage msg attrs
