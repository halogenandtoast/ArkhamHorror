module Arkham.Asset.Assets.DirectiveRedTape (directiveRedTape) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (PlayCard)
import Arkham.Asset.Import.Lifted qualified as Msg
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Helpers.Window (cardPlayed)
import Arkham.Matcher
import Control.Lens (non)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Lens (_Bool, _Integer)

newtype DirectiveRedTape = DirectiveRedTape AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

directiveRedTape :: AssetCard DirectiveRedTape
directiveRedTape = asset DirectiveRedTape Cards.directiveRedTape

instance HasModifiersFor DirectiveRedTape where
  getModifiersFor (DirectiveRedTape a) = case a.controller of
    Just iid | not a.flipped -> do
      let redTape = fromMaybe 0 $ a ^? metaMapL . ix "red_tape" . _Integer
      let ignored = fromMaybe False $ a ^? metaMapL . ix "ignore_regulation" . _Bool
      modified_ a iid
        $ [CanBecomeFast $ #event <> oneOf [#insight, #tactic] | a.ready]
        <> [CannotPlay AnyCard | redTape >= 2 && not ignored]
    _ -> pure mempty

instance HasAbilities DirectiveRedTape where
  getAbilities (DirectiveRedTape a) =
    [ restricted a 1 ControlsThis
        $ triggered
          (PlayCard #when You (basic $ #event <> oneOf [#insight, #tactic] <> not_ FastCard))
          (exhaust a)
    | not a.flipped
    ]

instance RunMessage DirectiveRedTape where
  runMessage msg a@(DirectiveRedTape attrs) = runQueueT $ case msg of
    Do BeginRound ->
      pure
        $ DirectiveRedTape
        $ attrs
        & (metaMapL %~ KeyMap.delete "ignore_regulation" . KeyMap.delete "red_tape")
    Msg.PlayCard iid _ _ _ _ False | attrs `controlledBy` iid -> do
      pure $ DirectiveRedTape $ attrs & metaMapL . at "red_tape" . non (Number 0) . _Integer +~ 1
    Flip _ _ (isTarget attrs -> True) -> do
      pure . DirectiveRedTape $ attrs & flippedL .~ True
    UseCardAbility _ (isSource attrs -> True) 1 (cardPlayed -> card) _ -> do
      costModifier (attrs.ability 1) card (BecomesFast FastPlayerWindow)
      pure a
    _ -> DirectiveRedTape <$> liftRunMessage msg attrs
