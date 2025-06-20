module Arkham.Asset.Assets.LibraryPass5 (libraryPass5) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Cost
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Placement

newtype LibraryPass5 = LibraryPass5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

libraryPass5 :: AssetCard LibraryPass5
libraryPass5 = asset LibraryPass5 Cards.libraryPass5

instance HasAbilities LibraryPass5 where
  getAbilities (LibraryPass5 x) =
    [ controlled
        x
        1
        ( DuringTurn You
            <> not_ (exists $ AssetAttachedToAsset (be x))
            <> exists (InHandOf NotForPlay You <> basic (#tome <> #asset))
        )
        $ FastAbility (exhaust x)
    , controlled
        x
        2
        (oneOf [youExist (InvestigatorWithResources $ atLeast 1), exists (AssetAttachedToAsset (be x))])
        $ forced
        $ TurnEnds #when You
    ]

instance RunMessage LibraryPass5 where
  runMessage msg a@(LibraryPass5 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      cards <- select $ inHandOf NotForPlay iid <> basic (#tome <> #asset)
      focusCards cards do
        chooseTargetM iid cards \card ->
          createAssetAt_ card $ AttachedToAsset attrs.id (Just $ InPlayArea iid)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      resources <- getSpendableResources iid
      selectOne (AssetAttachedToAsset $ be attrs) >>= \case
        Nothing -> spendResources iid 1
        Just aid -> chooseOrRunOneM iid do
          when (resources > 0) $ labeled "Pay 1 Resources" (spendResources iid 1)
          labeled "Put on bottom of deck" $ putOnBottomOfDeck iid iid aid
      pure a
    _ -> LibraryPass5 <$> liftRunMessage msg attrs
