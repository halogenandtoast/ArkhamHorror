module Arkham.Homebrew.DarkMatter.Assets.UniversalArchives (universalArchives) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (getScanningDeck, scanAction, scanAt, scanIcons)
import Arkham.Location.Types (Field (LocationPrintedSymbol))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype UniversalArchives = UniversalArchives AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

universalArchives :: AssetCard UniversalArchives
universalArchives = asset UniversalArchives Cards.universalArchives

{- | "[action] If the top card of the scanning deck contains an icon matching any
revealed location, exhaust Universal Archives: Scan. Scan as if you were at that
location. Shuffle the scanning deck."
-}
instance HasAbilities UniversalArchives where
  getAbilities (UniversalArchives a) =
    [controlled_ a 1 $ scanAction (exhaust a)]

instance RunMessage UniversalArchives where
  runMessage msg a@(UniversalArchives attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      investigators <- select $ colocatedWith iid
      chooseTargetM iid investigators \bearer -> putCardIntoPlay bearer attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      topIcons <- maybe [] scanIcons . listToMaybe <$> getScanningDeck
      revealed <- select RevealedLocation
      matching <- filterM (fmap (`elem` topIcons) . field LocationPrintedSymbol) revealed
      chooseTargetM iid matching \lid -> do
        symbol <- field LocationPrintedSymbol lid
        scanAt iid (attrs.ability 1) lid [symbol]
      pure a
    _ -> UniversalArchives <$> liftRunMessage msg attrs
