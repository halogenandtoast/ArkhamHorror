module Arkham.Event.Cards.SwiftReload2 (
  swiftReload2,
  SwiftReload2 (..),
)
where

import Arkham.Prelude

import Arkham.Asset.Types (Field (..))
import Arkham.Asset.Uses
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Use
import Arkham.Matcher
import Arkham.Projection
import Arkham.Trait (Trait (Firearm))

newtype SwiftReload2 = SwiftReload2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

swiftReload2 :: EventCard SwiftReload2
swiftReload2 = event SwiftReload2 Cards.swiftReload2

instance RunMessage SwiftReload2 where
  runMessage msg e@(SwiftReload2 attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      firearms <- selectList $ assetControlledBy iid <> AssetWithTrait Firearm <> AssetNotAtUsesX
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ targetLabel firearm [HandleTargetChoice iid (toSource attrs) (toTarget firearm)]
          | firearm <- firearms
          ]
      pure e
    HandleTargetChoice _ (isSource attrs -> True) (AssetTarget aid) -> do
      startingUses <- fmap useCount . asStartingUses =<< field AssetStartingUses aid
      currentUses <- fieldMap AssetUses (findWithDefault 0 Ammo) aid
      let diff = startingUses - currentUses
      push $ AddUses aid Ammo diff
      pure e
    _ -> SwiftReload2 <$> runMessage msg attrs
