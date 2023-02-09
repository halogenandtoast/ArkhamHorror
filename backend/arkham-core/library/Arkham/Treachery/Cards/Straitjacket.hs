module Arkham.Treachery.Cards.Straitjacket
  ( straitjacket
  , Straitjacket(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Assets
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Card
import Arkham.Card.PlayerCard
import Arkham.Classes
import Arkham.Matcher hiding (Discarded)
import Arkham.Message
import Arkham.Slot
import Arkham.Source
import Arkham.Target
import Arkham.Treachery.Runner

newtype Straitjacket = Straitjacket TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

straitjacket :: TreacheryCard Straitjacket
straitjacket = treachery Straitjacket Cards.straitjacket

instance RunMessage Straitjacket where
  runMessage msg t@(Straitjacket attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      alreadyInStraitJacket <-
        selectAny
        $ AssetControlledBy (InvestigatorWithId iid)
        <> assetIs Assets.straitjacket
      if alreadyInStraitJacket
        then t <$ push (Discard GameSource $ toTarget attrs)
        else do
          returnableAssets <-
            selectList
            $ AssetControlledBy (InvestigatorWithId iid)
            <> AssetCanLeavePlayByNormalMeans
            <> AssetOneOf [AssetInSlot BodySlot, AssetInSlot HandSlot]
          let asset = lookupPlayerCard Assets.straitjacket (toCardId attrs)
          t <$ pushAll
            (map (ReturnToHand iid . AssetTarget) returnableAssets
            <> [TakeControlOfSetAsideAsset iid (PlayerCard asset)]
            )
    After (Revelation _ source) | isSource attrs source ->
      t <$ push (Discarded (toTarget attrs) (toCard attrs)) -- Using discarded to remove existence)
    _ -> Straitjacket <$> runMessage msg attrs
