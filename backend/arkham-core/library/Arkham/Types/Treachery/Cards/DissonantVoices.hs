{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.DissonantVoices where

import Arkham.Import

import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype DissonantVoices= DissonantVoices Attrs
  deriving newtype (Show, ToJSON, FromJSON)

dissonantVoices :: TreacheryId -> a -> DissonantVoices
dissonantVoices uuid _ = DissonantVoices $ baseAttrs uuid "01165"

instance HasModifiersFor env DissonantVoices where
  getModifiersFor _ (InvestigatorTarget iid) (DissonantVoices attrs) = pure
    [ CannotPlay [AssetType, EventType] | treacheryOnInvestigator iid attrs ]
  getModifiersFor _ _ _ = pure []

instance HasActions env DissonantVoices where
  getActions i window (DissonantVoices attrs) = getActions i window attrs

instance (TreacheryRunner env) => RunMessage env DissonantVoices where
  runMessage msg t@(DissonantVoices attrs@Attrs {..}) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ unshiftMessage (AttachTreachery treacheryId (InvestigatorTarget iid))
    EndRound -> t <$ unshiftMessage (Discard $ toTarget attrs)
    _ -> DissonantVoices <$> runMessage msg attrs
