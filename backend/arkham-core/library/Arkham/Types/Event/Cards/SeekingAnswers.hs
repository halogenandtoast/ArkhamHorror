{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Cards.SeekingAnswers
  ( seekingAnswers
  , SeekingAnswers(..)
  )
where

import Arkham.Import

import Arkham.Types.Event.Attrs

newtype SeekingAnswers = SeekingAnswers Attrs
  deriving newtype (Show, ToJSON, FromJSON)

seekingAnswers :: InvestigatorId -> EventId -> SeekingAnswers
seekingAnswers iid uuid = SeekingAnswers $ baseAttrs iid uuid "02023"

instance HasActions env SeekingAnswers where
  getActions iid window (SeekingAnswers attrs) = getActions iid window attrs

instance HasModifiersFor env SeekingAnswers where
  getModifiersFor = noModifiersFor

instance (HasQueue env, HasId LocationId env InvestigatorId) => RunMessage env SeekingAnswers where
  runMessage msg e@(SeekingAnswers attrs@Attrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      lid <- getId @LocationId iid
      e <$ unshiftMessages
        [ CreateEffect
          "02023"
          Nothing
          (toSource attrs)
          (InvestigationTarget iid lid)
        , Discard (toTarget attrs)
        ]
    _ -> SeekingAnswers <$> runMessage msg attrs
