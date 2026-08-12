module Arkham.Homebrew.DarkMatter.Assets.EvaSuit (evaSuit) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (campaignI18n, scan, shuffleIntoScanningDeck)
import Arkham.Homebrew.DarkMatter.Traits (pattern Access)
import Arkham.LocationSymbol qualified as LS
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype EvaSuit = EvaSuit AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

evaSuit :: AssetCard EvaSuit
evaSuit = asset EvaSuit Cards.evaSuit

instance HasAbilities EvaSuit where
  getAbilities (EvaSuit a) =
    [controlled a 1 (exists $ YourLocation <> LocationWithTrait Access) doubleActionAbility]

instance RunMessage EvaSuit where
  runMessage msg a@(EvaSuit attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      investigators <- select Anyone
      chooseOneM iid $ campaignI18n do
        targets investigators (`putCardIntoPlay` attrs)
        labeled' "evaSuit.doNotPutIntoPlay" $ shuffleIntoScanningDeck [attrs]
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withLocationOf iid \current -> do
        destinations <- select $ LocationWithTrait Access <> not_ (be current)
        chooseOrRunOneM iid $ targets destinations \lid -> do
          moveTo (attrs.ability 1) iid lid
          scan iid (attrs.ability 1) [LS.Star]
      pure a
    _ -> EvaSuit <$> liftRunMessage msg attrs
