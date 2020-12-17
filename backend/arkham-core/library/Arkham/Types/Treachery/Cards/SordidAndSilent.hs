{-# LANGUAGE UndecidableInstances #-}

module Arkham.Types.Treachery.Cards.SordidAndSilent
  ( SordidAndSilent(..)
  , sordidAndSilent
  )
where

import Arkham.Import

import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype SordidAndSilent = SordidAndSilent Attrs
  deriving newtype (Show, ToJSON, FromJSON)

sordidAndSilent :: TreacheryId -> a -> SordidAndSilent
sordidAndSilent uuid _ = SordidAndSilent $ baseAttrs uuid "02089"

instance HasModifiersFor env SordidAndSilent where
  getModifiersFor = noModifiersFor

instance HasActions env SordidAndSilent where
  getActions i window (SordidAndSilent attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env SordidAndSilent where
  runMessage msg t@(SordidAndSilent attrs@Attrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      lid <- getId @LocationId iid
      t <$ unshiftMessage (AttachTreachery treacheryId $ LocationTarget lid)
    EndRound -> case treacheryAttachedTarget of
      Just (LocationTarget lid) -> do
        iids <- getSetList @InvestigatorId lid
        t <$ unshiftMessages
          [ InvestigatorAssignDamage iid (toSource attrs) 0 1 | iid <- iids ]
      _ -> pure t
    AdvanceAgenda _ -> t <$ unshiftMessage (Discard $ toTarget attrs)
    _ -> SordidAndSilent <$> runMessage msg attrs
