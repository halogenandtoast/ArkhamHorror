module Arkham.Asset.Assets.HuntersInstinct2 (huntersInstinct2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype HuntersInstinct2 = HuntersInstinct2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

huntersInstinct2 :: AssetCard HuntersInstinct2
huntersInstinct2 = asset HuntersInstinct2 Cards.huntersInstinct2

instance HasAbilities HuntersInstinct2 where
  getAbilities (HuntersInstinct2 a) =
    [ controlled
        a
        1
        ( youExist
            $ DiscardWith (HasCard $ #event <> mapOneOf CardWithLevel [0, 1, 2])
            <> noModifier CardsCannotLeaveYourDiscardPile
        )
        $ triggered (EnemyEngaged #after You AnyEnemy) (assetUseCost a Supply 1 <> exhaust a)
    ]

instance RunMessage HuntersInstinct2 where
  runMessage msg a@(HuntersInstinct2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      cards <- select $ inDiscardOf iid <> basic (#event <> mapOneOf CardWithLevel [0, 1, 2])
      chooseOneM iid $ targets cards $ addToHand iid . only
      pure a
    _ -> HuntersInstinct2 <$> liftRunMessage msg attrs
