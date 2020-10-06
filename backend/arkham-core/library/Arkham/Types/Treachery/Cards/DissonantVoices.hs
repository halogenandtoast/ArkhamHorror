{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.DissonantVoices where

import Arkham.Json
import Arkham.Types.Card.PlayerCard.Type
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner
import Arkham.Types.TreacheryId
import ClassyPrelude
import Lens.Micro

newtype DissonantVoices= DissonantVoices Attrs
  deriving newtype (Show, ToJSON, FromJSON)

dissonantVoices :: TreacheryId -> a -> DissonantVoices
dissonantVoices uuid _ = DissonantVoices $ baseAttrs uuid "01165"

instance HasActions env investigator DissonantVoices where
  getActions i window (DissonantVoices attrs) = getActions i window attrs

instance (TreacheryRunner env) => RunMessage env DissonantVoices where
  runMessage msg t@(DissonantVoices attrs@Attrs {..}) = case msg of
    Revelation iid tid | tid == treacheryId -> do
      unshiftMessages
        [ AttachTreachery tid (InvestigatorTarget iid)
        , AddModifiers
          (InvestigatorTarget iid)
          (TreacherySource tid)
          [CannotPlay [AssetType, EventType]]
        ]
      DissonantVoices <$> runMessage msg (attrs & attachedInvestigator ?~ iid)
    EndRound -> case treacheryAttachedInvestigator of
      Just iid -> t <$ unshiftMessages
        [ RemoveAllModifiersOnTargetFrom
          (InvestigatorTarget iid)
          (TreacherySource treacheryId)
        , Discard (TreacheryTarget treacheryId)
        ]
      Nothing -> pure t -- Note: This assumes the treachery never attached for some reason
    _ -> DissonantVoices <$> runMessage msg attrs
