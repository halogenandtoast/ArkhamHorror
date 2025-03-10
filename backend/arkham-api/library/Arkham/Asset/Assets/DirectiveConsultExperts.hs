module Arkham.Asset.Assets.DirectiveConsultExperts (directiveConsultExperts) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectWhen)
import Arkham.Matcher
import Arkham.Slot
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Lens (_Bool)

newtype DirectiveConsultExperts = DirectiveConsultExperts AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

directiveConsultExperts :: AssetCard DirectiveConsultExperts
directiveConsultExperts = asset DirectiveConsultExperts Cards.directiveConsultExperts

instance HasModifiersFor DirectiveConsultExperts where
  getModifiersFor (DirectiveConsultExperts a) = for_ a.controller \iid ->
    unless a.flipped do
      let ignored = fromMaybe False $ a ^? metaMapL . ix "ignore_regulation" . _Bool
      modifySelectWhen a (not ignored) (asset_ #ally) [CannotAssignDamage iid]

instance RunMessage DirectiveConsultExperts where
  runMessage msg (DirectiveConsultExperts attrs) = runQueueT $ case msg of
    Do BeginRound ->
      pure $ DirectiveConsultExperts $ attrs & metaMapL %~ KeyMap.delete "ignore_regulation"
    CardIsEnteringPlay iid card | toCardId card == toCardId attrs -> do
      push $ AddSlot iid AllySlot $ Slot (toSource attrs) []
      DirectiveConsultExperts <$> liftRunMessage msg attrs
    Flip _ _ (isTarget attrs -> True) -> do
      push $ SlotSourceRemovedFromPlay (AssetSource attrs.id)
      pure . DirectiveConsultExperts $ attrs & flippedL .~ True
    _ -> DirectiveConsultExperts <$> liftRunMessage msg attrs
