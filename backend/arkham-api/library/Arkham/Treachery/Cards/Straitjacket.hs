module Arkham.Treachery.Cards.Straitjacket (straitjacket) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Card.PlayerCard
import Arkham.Matcher hiding (Discarded)
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Straitjacket = Straitjacket TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

straitjacket :: TreacheryCard Straitjacket
straitjacket = treachery Straitjacket Cards.straitjacket

instance RunMessage Straitjacket where
  runMessage msg t@(Straitjacket attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      alreadyInStraitJacket <- selectAny $ assetControlledBy iid <> assetIs Assets.straitjacket
      unless alreadyInStraitJacket $ do
        returnableAssets <-
          select
            $ assetControlledBy iid
            <> AssetCanLeavePlayByNormalMeans
            <> mapOneOf AssetInSlot [#body, #hand]
        for_ returnableAssets (returnToHand iid)
        let asset = lookupPlayerCard Assets.straitjacket (toCardId attrs)
        takeControlOfSetAsideAsset iid (PlayerCard asset)
      pure t
    After (Revelation _ (isSource attrs -> True)) -> do
      push $ Discarded (toTarget attrs) (toSource attrs) (toCard attrs) -- Using discarded to remove existence)
      pure t
    _ -> Straitjacket <$> liftRunMessage msg attrs
