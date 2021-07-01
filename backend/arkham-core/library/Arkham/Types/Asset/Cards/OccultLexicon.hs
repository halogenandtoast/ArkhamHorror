module Arkham.Types.Asset.Cards.OccultLexicon where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.PlayerCard
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Message

newtype OccultLexicon = OccultLexicon AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

occultLexicon :: AssetCard OccultLexicon
occultLexicon = hand OccultLexicon Cards.occultLexicon

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
