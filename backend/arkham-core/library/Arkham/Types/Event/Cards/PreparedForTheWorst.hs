module Arkham.Types.Event.Cards.PreparedForTheWorst
  ( preparedForTheWorst
  , PreparedForTheWorst(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Trait

newtype PreparedForTheWorst = PreparedForTheWorst EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

preparedForTheWorst :: EventCard PreparedForTheWorst
preparedForTheWorst = event PreparedForTheWorst Cards.preparedForTheWorst

instance HasActions env PreparedForTheWorst where
  getActions iid window (PreparedForTheWorst attrs) =
    getActions iid window attrs

instance HasModifiersFor env PreparedForTheWorst where
  getModifiersFor = noModifiersFor

instance RunMessage env PreparedForTheWorst where
  runMessage msg e@(PreparedForTheWorst attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ | eid == toId attrs -> do
      e <$ pushAll
        [ SearchTopOfDeck
          iid
          (toSource attrs)
          (InvestigatorTarget iid)
          9
          [Weapon]
          (ShuffleBackIn $ DrawFound iid)
        , Discard (toTarget attrs)
        ]
    _ -> PreparedForTheWorst <$> runMessage msg attrs
