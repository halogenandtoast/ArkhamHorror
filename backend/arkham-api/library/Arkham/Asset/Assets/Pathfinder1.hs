module Arkham.Asset.Assets.Pathfinder1 (pathfinder1) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Location
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype Pathfinder1 = Pathfinder1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pathfinder1 :: AssetCard Pathfinder1
pathfinder1 = asset Pathfinder1 Cards.pathfinder1

instance HasAbilities Pathfinder1 where
  getAbilities (Pathfinder1 attrs) =
    [ controlled
        attrs
        1
        ( youExist (UnengagedInvestigator <> InvestigatorCanMove)
            <> exists AccessibleLocation
            <> DuringTurn You
        )
        $ FastAbility (exhaust attrs)
    ]

instance RunMessage Pathfinder1 where
  runMessage msg a@(Pathfinder1 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <- getAccessibleLocations iid (attrs.ability 1)
      chooseTargetM iid locations $ moveTo (attrs.ability 1) iid
      pure a
    _ -> Pathfinder1 <$> liftRunMessage msg attrs
