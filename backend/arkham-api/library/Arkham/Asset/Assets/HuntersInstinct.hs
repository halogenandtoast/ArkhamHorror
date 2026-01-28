module Arkham.Asset.Assets.HuntersInstinct (huntersInstinct) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype HuntersInstinct = HuntersInstinct AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

huntersInstinct :: AssetCard HuntersInstinct
huntersInstinct = assetWith HuntersInstinct Cards.huntersInstinct (whenNoUsesL ?~ DiscardWhenNoUses)

instance HasAbilities HuntersInstinct where
  getAbilities (HuntersInstinct a) =
    [ controlled
        a
        1
        ( youExist
            $ DiscardWith (HasCard $ #event <> CardWithLevel 0)
            <> noModifier CardsCannotLeaveYourDiscardPile
        )
        $ triggered (EnemyEngaged #after You AnyEnemy) (assetUseCost a Supply 1 <> exhaust a)
    ]

instance RunMessage HuntersInstinct where
  runMessage msg a@(HuntersInstinct attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      cards <- select $ inDiscardOf iid <> basic (#event <> CardWithLevel 0)
      chooseOneM iid $ targets cards $ addToHand iid . only
      pure a
    _ -> HuntersInstinct <$> liftRunMessage msg attrs
