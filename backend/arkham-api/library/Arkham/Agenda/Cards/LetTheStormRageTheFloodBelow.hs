module Arkham.Agenda.Cards.LetTheStormRageTheFloodBelow (
  LetTheStormRageTheFloodBelow (..),
  letTheStormRageTheFloodBelow,
) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Helpers
import Arkham.Agenda.Runner
import Arkham.Card
import Arkham.Classes
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.GameValue
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Treacheries

newtype LetTheStormRageTheFloodBelow = LetTheStormRageTheFloodBelow AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

letTheStormRageTheFloodBelow :: AgendaCard LetTheStormRageTheFloodBelow
letTheStormRageTheFloodBelow =
  agenda
    (2, A)
    LetTheStormRageTheFloodBelow
    Cards.letTheStormRageTheFloodBelow
    (Static 6)

instance HasModifiersFor LetTheStormRageTheFloodBelow where
  getModifiersFor (LetTheStormRageTheFloodBelow a) = do
    ancientEvils <- findAllCards (`isCard` Treacheries.ancientEvils)
    modifyEach a ancientEvils [AddKeyword Keyword.Surge]

instance HasAbilities LetTheStormRageTheFloodBelow where
  getAbilities (LetTheStormRageTheFloodBelow a) =
    [groupLimit PerRound $ mkAbility a 1 $ FastAbility $ GroupClueCost (PerPlayer 1) Anywhere]

instance RunMessage LetTheStormRageTheFloodBelow where
  runMessage msg a@(LetTheStormRageTheFloodBelow attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      openThePathBelow <- getSetAsideCard Acts.openThePathBelow
      pushAll [toDiscard GameSource attrs, AddAct 1 openThePathBelow]
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      investigatorIds <- getInvestigators
      pushAll
        $ [PlaceDoom (toAbilitySource attrs 1) (toTarget attrs) 1, AdvanceAgendaIfThresholdSatisfied]
        <> [ TakeResources iid 2 (toAbilitySource attrs 1) False
           | iid <- investigatorIds
           ]
      pure a
    _ -> LetTheStormRageTheFloodBelow <$> runMessage msg attrs
