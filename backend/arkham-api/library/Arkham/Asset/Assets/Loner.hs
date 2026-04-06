module Arkham.Asset.Assets.Loner (loner) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.ForMovement
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype Loner = Loner AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

loner :: AssetCard Loner
loner = asset Loner Cards.loner

instance HasAbilities Loner where
  getAbilities (Loner a) =
    [ controlled
        a
        1
        ( DuringTurn You
            <> exists
              ( CanMoveToLocation You (a.ability 1)
                  $ EmptyLocation
                  <> ConnectedLocation NotForMovement
              )
        )
        $ freeTrigger (exhaust a)
    ]

instance RunMessage Loner where
  runMessage msg a@(Loner attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      lids <-
        select
          $ CanMoveToLocation (InvestigatorWithId iid) (attrs.ability 1)
          $ EmptyLocation
          <> ConnectedLocation NotForMovement
      chooseOrRunOneM iid $ targets lids $ moveTo attrs iid
      pure a
    _ -> Loner <$> liftRunMessage msg attrs
