module Arkham.Types.Event.Cards.PreparedForTheWorst
  ( preparedForTheWorst
  , PreparedForTheWorst(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Types.Card.CardType
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Trait

newtype PreparedForTheWorst = PreparedForTheWorst EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

preparedForTheWorst :: EventCard PreparedForTheWorst
preparedForTheWorst = event PreparedForTheWorst Cards.preparedForTheWorst

instance RunMessage env PreparedForTheWorst where
  runMessage msg e@(PreparedForTheWorst attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      e <$ pushAll
        [ Search
          iid
          (toSource attrs)
          (InvestigatorTarget iid)
          [fromTopOfDeck 9]
          (CardWithType AssetType <> CardWithTrait Weapon)
          (DrawFound iid 1)
        , Discard (toTarget attrs)
        ]
    _ -> PreparedForTheWorst <$> runMessage msg attrs
