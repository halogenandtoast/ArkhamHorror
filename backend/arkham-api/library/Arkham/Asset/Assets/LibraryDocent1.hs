module Arkham.Asset.Assets.LibraryDocent1 (libraryDocent1, LibraryDocent1 (..)) where

import Arkham.Ability hiding (DuringTurn)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher hiding (DuringTurn, FastPlayerWindow)
import Arkham.Message.Lifted.Choose
import Arkham.Name
import Arkham.Projection
import Arkham.Trait (Trait (Tome))
import Data.Monoid (First (..))

newtype LibraryDocent1 = LibraryDocent1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

libraryDocent1 :: AssetCard LibraryDocent1
libraryDocent1 = ally LibraryDocent1 Cards.libraryDocent1 (1, 2)

instance HasAbilities LibraryDocent1 where
  getAbilities (LibraryDocent1 a) =
    [ controlledAbility
        a
        1
        ( PlayableCardExistsWithCostReduction
            (Reduce 2)
            (HandCardWithDifferentTitleFromAtLeastOneAsset You #tome #tome)
        )
        $ ReactionAbility (AssetEntersPlay #when (be a))
        $ ReturnMatchingAssetToHandCost
        $ AssetWithDifferentTitleFromAtLeastOneCardInHand You (basic #tome) #tome
    ]

getAssetPayment :: Payment -> Maybe Card
getAssetPayment (ReturnToHandPayment c) = Just c
getAssetPayment (Payments ps) = ala First foldMap $ map getAssetPayment ps
getAssetPayment _ = Nothing

instance RunMessage LibraryDocent1 where
  runMessage msg a@(LibraryDocent1 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (getAssetPayment -> Just assetPayment) -> do
      handCards <- field InvestigatorHand iid
      let
        targetCards =
          filterBy
            [ (`cardMatch` (CardWithType AssetType <> CardWithTrait Tome))
            , (/= (toName assetPayment)) . toName
            ]
            handCards
      chooseTargetM iid targetCards \tome -> do
        reduceCostOf attrs tome 2
        playCardPayingCost iid tome
      pure a
    _ -> LibraryDocent1 <$> liftRunMessage msg attrs
