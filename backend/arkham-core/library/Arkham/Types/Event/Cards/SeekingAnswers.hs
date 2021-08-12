module Arkham.Types.Event.Cards.SeekingAnswers
  ( seekingAnswers
  , SeekingAnswers(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Target

newtype SeekingAnswers = SeekingAnswers EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

seekingAnswers :: EventCard SeekingAnswers
seekingAnswers = event SeekingAnswers Cards.seekingAnswers

instance HasActions SeekingAnswers
instance HasModifiersFor env SeekingAnswers

instance (HasQueue env, HasId LocationId env InvestigatorId) => RunMessage env SeekingAnswers where
  runMessage msg e@(SeekingAnswers attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ | eid == eventId -> do
      lid <- getId @LocationId iid
      e <$ pushAll
        [ CreateEffect
          "02023"
          Nothing
          (toSource attrs)
          (InvestigationTarget iid lid)
        , Discard (toTarget attrs)
        ]
    _ -> SeekingAnswers <$> runMessage msg attrs
