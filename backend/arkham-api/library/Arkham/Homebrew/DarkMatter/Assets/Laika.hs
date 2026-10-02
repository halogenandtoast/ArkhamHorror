module Arkham.Homebrew.DarkMatter.Assets.Laika (laika) where

import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (SanityModifier), controllerGets)
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype Laika = Laika AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

laika :: AssetCard Laika
laika = ally Laika Cards.laika (1, 1)

instance HasModifiersFor Laika where
  getModifiersFor (Laika a) = controllerGets a [SanityModifier 2]

{- | "Revelation - Put this card into play under the control of an investigator at
your location. / You get +2 sanity."
-}
instance RunMessage Laika where
  runMessage msg a@(Laika attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      investigators <- select $ colocatedWith iid
      chooseTargetM iid investigators (`putCardIntoPlay` attrs)
      pure a
    _ -> Laika <$> liftRunMessage msg attrs
