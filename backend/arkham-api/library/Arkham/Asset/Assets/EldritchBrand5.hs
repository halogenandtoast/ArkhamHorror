module Arkham.Asset.Assets.EldritchBrand5 (eldritchBrand5) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Name
import Arkham.Strategy

newtype EldritchBrand5 = EldritchBrand5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eldritchBrand5 :: AssetCard EldritchBrand5
eldritchBrand5 = asset EldritchBrand5 Cards.eldritchBrand5

instance HasAbilities EldritchBrand5 where
  getAbilities (EldritchBrand5 a) =
    [restricted a 1 ControlsThis $ freeReaction $ DrawingStartingHand #when You]

instance RunMessage EldritchBrand5 where
  runMessage msg a@(EldritchBrand5 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      attachments <- mapMaybe (fmap toTitle . lookupCardDef) <$> getCardAttachments iid attrs
      search iid attrs iid [fromDeck] (basic $ mapOneOf CardWithTitle attachments) (PlayFound iid 1)
      pure a
    _ -> EldritchBrand5 <$> liftRunMessage msg attrs
