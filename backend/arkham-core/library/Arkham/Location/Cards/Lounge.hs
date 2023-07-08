module Arkham.Location.Cards.Lounge (
  lounge,
  Lounge (..),
)
where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Assets
import Arkham.GameValue
import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Placement
import Arkham.Scenarios.ForTheGreaterGood.Helpers
import Arkham.Timing qualified as Timing

newtype Lounge = Lounge LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lounge :: LocationCard Lounge
lounge = location Lounge Cards.lounge 2 (PerPlayer 2)

instance HasAbilities Lounge where
  getAbilities (Lounge attrs) =
    withRevealedAbilities
      attrs
      [mkAbility attrs 1 $ ForcedAbility $ RevealLocation Timing.After You $ LocationWithId $ toId attrs]

instance RunMessage Lounge where
  runMessage msg l@(Lounge attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      augustLindquist <- getSetAsideCard Assets.augustLindquist
      augustLindquistId <- getRandom
      k <- getRandomKey
      pushAll
        [ PlaceLocationMatching (CardWithTitle "Vault")
        , PlaceLocationMatching (CardWithTitle "Library")
        , CreateAssetAt augustLindquistId augustLindquist (AtLocation $ toId attrs)
        , PlaceKey (AssetTarget augustLindquistId) k
        ]
      pure l
    _ -> Lounge <$> runMessage msg attrs
