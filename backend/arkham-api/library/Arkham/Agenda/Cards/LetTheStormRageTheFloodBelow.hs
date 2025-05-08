module Arkham.Agenda.Cards.LetTheStormRageTheFloodBelow (letTheStormRageTheFloodBelow) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Card
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Treacheries

newtype LetTheStormRageTheFloodBelow = LetTheStormRageTheFloodBelow AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

letTheStormRageTheFloodBelow :: AgendaCard LetTheStormRageTheFloodBelow
letTheStormRageTheFloodBelow =
  agenda (2, A) LetTheStormRageTheFloodBelow Cards.letTheStormRageTheFloodBelow (Static 6)

instance HasModifiersFor LetTheStormRageTheFloodBelow where
  getModifiersFor (LetTheStormRageTheFloodBelow a) = do
    ancientEvils <- findAllCards (`isCard` Treacheries.ancientEvils)
    modifyEach a ancientEvils [AddKeyword Keyword.Surge]

instance HasAbilities LetTheStormRageTheFloodBelow where
  getAbilities (LetTheStormRageTheFloodBelow a) =
    [groupLimit PerRound $ mkAbility a 1 $ FastAbility $ GroupClueCost (PerPlayer 1) Anywhere]

instance RunMessage LetTheStormRageTheFloodBelow where
  runMessage msg a@(LetTheStormRageTheFloodBelow attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      toDiscard GameSource attrs
      openThePathBelow <- getSetAsideCard Acts.openThePathBelow
      push $ AddAct 1 openThePathBelow
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoom (attrs.ability 1) attrs 1
      push AdvanceAgendaIfThresholdSatisfied
      eachInvestigator \iid -> gainResources iid (attrs.ability 1) 2
      pure a
    _ -> LetTheStormRageTheFloodBelow <$> liftRunMessage msg attrs
