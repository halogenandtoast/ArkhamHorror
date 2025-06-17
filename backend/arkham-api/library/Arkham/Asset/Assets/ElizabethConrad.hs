module Arkham.Asset.Assets.ElizabethConrad (elizabethConrad) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Location (getAccessibleLocations)
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move (moveTo)

newtype ElizabethConrad = ElizabethConrad AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

elizabethConrad :: AssetCard ElizabethConrad
elizabethConrad = allyWith ElizabethConrad Cards.elizabethConrad (1, 3) noSlots

instance HasAbilities ElizabethConrad where
  getAbilities (ElizabethConrad a) =
    [ controlled a 1 (DuringTurn You)
        $ triggered (DrawsCards #after You AnyCards AnyValue) (exhaust a)
    ]

instance RunMessage ElizabethConrad where
  runMessage msg a@(ElizabethConrad attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- select $ colocatedWith iid
      chooseOneM iid $ targets investigators \iid' -> do
        locations <- getAccessibleLocations iid' attrs
        chooseOneM iid' $ targets locations $ \lid ->
          moveTo (attrs.ability 1) iid' lid
      pure a
    _ -> ElizabethConrad <$> liftRunMessage msg attrs
