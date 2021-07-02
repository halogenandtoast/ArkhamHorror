module Arkham.Types.Treachery.Cards.AbandonedAndAlone where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards (abandonedAndAlone)
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype AbandonedAndAlone = AbandonedAndAlone TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

abandonedAndAlone :: TreacheryCard AbandonedAndAlone
abandonedAndAlone = treachery AbandonedAndAlone Cards.abandonedAndAlone

instance HasModifiersFor env AbandonedAndAlone where
  getModifiersFor = noModifiersFor

instance HasActions env AbandonedAndAlone where
  getActions i window (AbandonedAndAlone attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env AbandonedAndAlone where
  runMessage msg t@(AbandonedAndAlone attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      t <$ unshiftMessages
        [ InvestigatorDirectDamage iid source 0 2
        , RemoveDiscardFromGame iid
        , Discard $ toTarget attrs
        ]
    _ -> AbandonedAndAlone <$> runMessage msg attrs
