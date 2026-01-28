module Arkham.Asset.Assets.Jumpsuit (jumpsuit) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype Jumpsuit = Jumpsuit AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jumpsuit :: AssetCard Jumpsuit
jumpsuit = assetWith Jumpsuit Cards.jumpsuit (healthL ?~ 2)

instance HasAbilities Jumpsuit where
  getAbilities (Jumpsuit a) =
    [ controlled
        a
        1
        ( DuringTurn You
            <> youExist
              ( DiscardWith (HasCard $ #asset <> oneOf [#weapon, #tool])
                  <> noModifier CardsCannotLeaveYourDiscardPile
              )
        )
        $ freeTrigger (discardCost a)
    ]

instance RunMessage Jumpsuit where
  runMessage msg a@(Jumpsuit attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      cards <- select $ inDiscardOf iid <> basic (#asset <> oneOf [#weapon, #tool])
      chooseOneM iid $ targets cards $ addToHand iid . only
      pure a
    _ -> Jumpsuit <$> liftRunMessage msg attrs
