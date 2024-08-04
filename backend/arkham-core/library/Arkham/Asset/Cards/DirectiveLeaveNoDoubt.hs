module Arkham.Asset.Cards.DirectiveLeaveNoDoubt (
  directiveLeaveNoDoubt,
  DirectiveLeaveNoDoubt (..),
)
where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified)
import Arkham.Investigator.Meta.RolandBanksParallel
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection

newtype DirectiveLeaveNoDoubt = DirectiveLeaveNoDoubt AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

directiveLeaveNoDoubt :: AssetCard DirectiveLeaveNoDoubt
directiveLeaveNoDoubt = asset DirectiveLeaveNoDoubt Cards.directiveLeaveNoDoubt

instance HasModifiersFor DirectiveLeaveNoDoubt where
  getModifiersFor (InvestigatorTarget iid) (DirectiveLeaveNoDoubt a) = do
    maybeModified a do
      guard $ not a.flipped
      guard $ a.controller == Just iid
      meta <- lift $ fieldMap InvestigatorMeta (toResultDefault defaultMeta) iid
      pure $ SanityModifier 3
        : [CannotMove | leaveNoDoubt meta >= 2 && "leaveNoDoubt" `notElem` ignoredDirectives meta]
  getModifiersFor _ _ = pure []

instance RunMessage DirectiveLeaveNoDoubt where
  runMessage msg (DirectiveLeaveNoDoubt attrs) = runQueueT $ case msg of
    Flip _ _ (isTarget attrs -> True) -> do
      pure . DirectiveLeaveNoDoubt $ attrs & flippedL .~ True
    _ -> DirectiveLeaveNoDoubt <$> liftRunMessage msg attrs
