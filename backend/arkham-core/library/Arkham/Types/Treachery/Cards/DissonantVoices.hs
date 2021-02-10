module Arkham.Types.Treachery.Cards.DissonantVoices where

import Arkham.Prelude

import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Target
import Arkham.Types.TreacheryId


import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Helpers
import Arkham.Types.Treachery.Runner

newtype DissonantVoices= DissonantVoices TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dissonantVoices :: TreacheryId -> a -> DissonantVoices
dissonantVoices uuid _ = DissonantVoices $ baseAttrs uuid "01165"

instance HasModifiersFor env DissonantVoices where
  getModifiersFor _ (InvestigatorTarget iid) (DissonantVoices attrs) =
    pure $ toModifiers
      attrs
      [ CannotPlay [(AssetType, mempty), (EventType, mempty)]
      | treacheryOnInvestigator iid attrs
      ]
  getModifiersFor _ _ _ = pure []

instance HasActions env DissonantVoices where
  getActions i window (DissonantVoices attrs) = getActions i window attrs

instance (TreacheryRunner env) => RunMessage env DissonantVoices where
  runMessage msg t@(DissonantVoices attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ unshiftMessage (AttachTreachery treacheryId (InvestigatorTarget iid))
    EndRound -> t <$ unshiftMessage (Discard $ toTarget attrs)
    _ -> DissonantVoices <$> runMessage msg attrs
