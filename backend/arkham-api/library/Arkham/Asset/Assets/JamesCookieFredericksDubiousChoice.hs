module Arkham.Asset.Assets.JamesCookieFredericksDubiousChoice
  ( jamesCookieFredericksDubiousChoice
  , JamesCookieFredericksDubiousChoice(..)
  )
where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype JamesCookieFredericksDubiousChoice = JamesCookieFredericksDubiousChoice AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jamesCookieFredericksDubiousChoice :: AssetCard JamesCookieFredericksDubiousChoice
jamesCookieFredericksDubiousChoice = allyWith JamesCookieFredericksDubiousChoice Cards.jamesCookieFredericksDubiousChoice (5, 1) noSlots

instance RunMessage JamesCookieFredericksDubiousChoice where
  runMessage msg (JamesCookieFredericksDubiousChoice attrs) = runQueueT $ case msg of
    _ -> JamesCookieFredericksDubiousChoice <$> liftRunMessage msg attrs
