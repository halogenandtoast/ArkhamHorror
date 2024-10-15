module Arkham.Location.Cards.Lounge (lounge, Lounge (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.GameValue
import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Placement
import Arkham.Scenarios.ForTheGreaterGood.Helpers

newtype Lounge = Lounge LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lounge :: LocationCard Lounge
lounge = location Lounge Cards.lounge 2 (PerPlayer 2)

instance HasAbilities Lounge where
  getAbilities (Lounge a) = extendRevealed1 a $ mkAbility a 1 $ forced $ RevealLocation #after You (be a)

instance RunMessage Lounge where
  runMessage msg l@(Lounge attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ PlaceLocationMatching (CardWithTitle "Vault")
      whenM (selectNone $ LocationWithTitle "Library") do
        push $ PlaceLocationMatching (CardWithTitle "Library")

      card <- getSetAsideCard Assets.augustLindquist
      augustLindquist <- createAssetAt card (AtLocation attrs.id)

      mKey <- getRandomKey
      for_ mKey $ placeKey augustLindquist
      pure l
    _ -> Lounge <$> liftRunMessage msg attrs
