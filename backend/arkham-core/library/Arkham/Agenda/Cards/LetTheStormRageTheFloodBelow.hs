module Arkham.Agenda.Cards.LetTheStormRageTheFloodBelow
  ( LetTheStormRageTheFloodBelow(..)
  , letTheStormRageTheFloodBelow
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Helpers
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.Cost
import Arkham.GameValue
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Message
import Arkham.Target
import Arkham.Treachery.Cards qualified as Treacheries

newtype LetTheStormRageTheFloodBelow = LetTheStormRageTheFloodBelow AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

letTheStormRageTheFloodBelow :: AgendaCard LetTheStormRageTheFloodBelow
letTheStormRageTheFloodBelow = agenda
  (2, A)
  LetTheStormRageTheFloodBelow
  Cards.letTheStormRageTheFloodBelow
  (Static 6)

instance HasModifiersFor LetTheStormRageTheFloodBelow where
  getModifiersFor (TreacheryTarget tid) (LetTheStormRageTheFloodBelow a) = do
    isAncientEvils <- tid <=~> treacheryIs Treacheries.ancientEvils
    pure $ toModifiers a [ AddKeyword Keyword.Surge | isAncientEvils ]
  getModifiersFor _ _ = pure []

instance HasAbilities LetTheStormRageTheFloodBelow where
  getAbilities (LetTheStormRageTheFloodBelow a) =
    [ limitedAbility (GroupLimit PerRound 1)
        $ mkAbility a 1
        $ FastAbility
        $ GroupClueCost (PerPlayer 1) Anywhere
    ]

instance RunMessage LetTheStormRageTheFloodBelow where
  runMessage msg a@(LetTheStormRageTheFloodBelow attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      pushAll
        [Discard (AgendaTarget $ toId attrs), AddAct 1 Acts.openThePathBelow]
      pure a
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      investigatorIds <- getInvestigatorIds
      pushAll
        $ [PlaceDoom (toTarget attrs) 1, AdvanceAgendaIfThresholdSatisfied]
        <> [ TakeResources iid 2 False | iid <- investigatorIds ]
      pure a
    _ -> LetTheStormRageTheFloodBelow <$> runMessage msg attrs
