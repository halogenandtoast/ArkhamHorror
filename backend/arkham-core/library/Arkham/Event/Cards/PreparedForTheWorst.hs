module Arkham.Event.Cards.PreparedForTheWorst (
  preparedForTheWorst,
  PreparedForTheWorst (..),
) where

import Arkham.Prelude

import Arkham.Card.CardType
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Trait

newtype PreparedForTheWorst = PreparedForTheWorst EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

preparedForTheWorst :: EventCard PreparedForTheWorst
preparedForTheWorst = event PreparedForTheWorst Cards.preparedForTheWorst

instance RunMessage PreparedForTheWorst where
  runMessage msg e@(PreparedForTheWorst attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      e
        <$ pushAll
          [ search
              iid
              (toSource attrs)
              (InvestigatorTarget iid)
              [fromTopOfDeck 9]
              (CardWithType AssetType <> CardWithTrait Weapon)
              (DrawFound iid 1)
          ]
    _ -> PreparedForTheWorst <$> runMessage msg attrs
