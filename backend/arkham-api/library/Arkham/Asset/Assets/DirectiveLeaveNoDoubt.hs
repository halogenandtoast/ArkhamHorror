module Arkham.Asset.Assets.DirectiveLeaveNoDoubt (
  directiveLeaveNoDoubt,
  DirectiveLeaveNoDoubt (..),
)
where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Investigator.Meta.RolandBanksParallel
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection

newtype DirectiveLeaveNoDoubt = DirectiveLeaveNoDoubt AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

directiveLeaveNoDoubt :: AssetCard DirectiveLeaveNoDoubt
directiveLeaveNoDoubt = asset DirectiveLeaveNoDoubt Cards.directiveLeaveNoDoubt

instance HasModifiersFor DirectiveLeaveNoDoubt where
  getModifiersFor (DirectiveLeaveNoDoubt a) = case a.controller of
    Just iid | not a.flipped -> do
      meta <- fieldMap InvestigatorMeta (toResultDefault defaultMeta) iid
      modified_ a iid
        $ SanityModifier 3
        : [CannotMove | leaveNoDoubt meta >= 2 && "leaveNoDoubt" `notElem` ignoredDirectives meta]
    _ -> pure mempty

instance RunMessage DirectiveLeaveNoDoubt where
  runMessage msg (DirectiveLeaveNoDoubt attrs) = runQueueT $ case msg of
    Flip _ _ (isTarget attrs -> True) -> do
      pure . DirectiveLeaveNoDoubt $ attrs & flippedL .~ True
    _ -> DirectiveLeaveNoDoubt <$> liftRunMessage msg attrs
