module Arkham.Types.Treachery.Cards.Straitjacket
  ( straitjacket
  , Straitjacket(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Assets
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Types.Card
import Arkham.Types.Card.PlayerCard
import Arkham.Types.Classes
import Arkham.Types.Matcher hiding (Discarded)
import Arkham.Types.Message
import Arkham.Types.Slot
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype Straitjacket = Straitjacket TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

straitjacket :: TreacheryCard Straitjacket
straitjacket = treachery Straitjacket Cards.straitjacket

instance TreacheryRunner env => RunMessage env Straitjacket where
  runMessage msg t@(Straitjacket attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      alreadyInStraitJacket <-
        selectAny
        $ AssetOwnedBy (InvestigatorWithId iid)
        <> assetIs Assets.straitjacket
      if alreadyInStraitJacket
        then t <$ push (Discard $ toTarget attrs)
        else do
          returnableAssets <-
            selectList
            $ AssetOwnedBy (InvestigatorWithId iid)
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
