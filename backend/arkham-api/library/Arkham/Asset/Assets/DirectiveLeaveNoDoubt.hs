module Arkham.Asset.Assets.DirectiveLeaveNoDoubt (directiveLeaveNoDoubt) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Movement
import Control.Lens (non)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Lens (_Bool, _Integer)

newtype DirectiveLeaveNoDoubt = DirectiveLeaveNoDoubt AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

directiveLeaveNoDoubt :: AssetCard DirectiveLeaveNoDoubt
directiveLeaveNoDoubt = asset DirectiveLeaveNoDoubt Cards.directiveLeaveNoDoubt

instance HasModifiersFor DirectiveLeaveNoDoubt where
  getModifiersFor (DirectiveLeaveNoDoubt a) = for_ a.controller \iid ->
    unless a.flipped do
      let ignored = fromMaybe False $ a ^? metaMapL . ix "ignore_regulation" . _Bool
      let leaveNoDoubt = fromMaybe 0 $ a ^? metaMapL . ix "leave_no_doubt" . _Integer
      modified_ a iid $ SanityModifier 3 : [CannotMove | leaveNoDoubt >= 2 && not ignored]

instance RunMessage DirectiveLeaveNoDoubt where
  runMessage msg a@(DirectiveLeaveNoDoubt attrs) = runQueueT $ case msg of
    Do BeginRound ->
      pure
        $ DirectiveLeaveNoDoubt
        $ attrs
        & (metaMapL %~ KeyMap.delete "ignore_regulation" . KeyMap.delete "leave_no_doubt")
    Flip _ _ (isTarget attrs -> True) -> do
      pure . DirectiveLeaveNoDoubt $ attrs & flippedL .~ True
    MoveTo movement | maybe False (`isTarget` movement.target) attrs.controller -> do
      case movement.destination of
        ToLocation _ ->
          pure
            $ DirectiveLeaveNoDoubt
            $ if movement.means /= Place
              then attrs & (metaMapL . at "leave_no_doubt" . non (Number 0) . _Integer +~ 1)
              else attrs
        _ -> pure a
    _ -> DirectiveLeaveNoDoubt <$> liftRunMessage msg attrs
