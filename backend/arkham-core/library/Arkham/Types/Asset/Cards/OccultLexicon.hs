module Arkham.Types.Asset.Cards.OccultLexicon where

import Arkham.Prelude

import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Slot
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner

newtype OccultLexicon = OccultLexicon AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

occultLexicon :: AssetId -> OccultLexicon
occultLexicon uuid =
  OccultLexicon $ (baseAttrs uuid "05316") { assetSlots = [HandSlot] }

instance HasModifiersFor env OccultLexicon where
  getModifiersFor = noModifiersFor

instance HasActions env OccultLexicon where
  getActions i window (OccultLexicon x) = getActions i window x

instance (AssetRunner env) => RunMessage env OccultLexicon where
  runMessage msg (OccultLexicon attrs) = case msg of
    InvestigatorPlayAsset iid aid _ _ | aid == assetId attrs -> do
      handBloodRite <- PlayerCard <$> genPlayerCard "05317"
      deckBloodRites <- replicateM 2 (genPlayerCard "05317")
      unshiftMessages
        [AddToHand iid handBloodRite, ShuffleCardsIntoDeck iid deckBloodRites]
      OccultLexicon <$> runMessage msg attrs
    RemovedFromPlay source | isSource attrs source -> do
      for_ (assetInvestigator attrs)
        $ \iid -> unshiftMessage (RemoveAllCopiesOfCardFromGame iid "05317")
      OccultLexicon <$> runMessage msg attrs
    _ -> OccultLexicon <$> runMessage msg attrs
