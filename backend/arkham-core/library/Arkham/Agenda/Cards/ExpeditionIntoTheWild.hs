module Arkham.Agenda.Cards.ExpeditionIntoTheWild (ExpeditionIntoTheWild (..), expeditionIntoTheWild) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.EncounterSet
import Arkham.GameValue
import Arkham.Helpers.Investigator
import Arkham.Helpers.Location
import Arkham.Matcher
import Arkham.Prelude

newtype ExpeditionIntoTheWild = ExpeditionIntoTheWild AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

expeditionIntoTheWild :: AgendaCard ExpeditionIntoTheWild
expeditionIntoTheWild = agenda (1, A) ExpeditionIntoTheWild Cards.expeditionIntoTheWild (Static 6)

instance HasAbilities ExpeditionIntoTheWild where
  getAbilities (ExpeditionIntoTheWild a) = [mkAbility a 1 exploreAction_]

instance RunMessage ExpeditionIntoTheWild where
  runMessage msg a@(ExpeditionIntoTheWild attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locationSymbols <- toConnections =<< getJustLocation iid
      let source = toAbilitySource attrs 1
      push $ Explore iid source (oneOf $ map CardWithPrintedLocationSymbol locationSymbols)
      pure a
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      setAsideAgentsOfYig <- getSetAsideEncounterSet AgentsOfYig
      iids <- getInvestigatorIds
      pushAll
        $ [ ShuffleEncounterDiscardBackIn
          , ShuffleCardsIntoDeck Deck.EncounterDeck setAsideAgentsOfYig
          ]
        <> [beginSkillTest iid attrs iid #willpower 3 | iid <- iids]
        <> [advanceAgendaDeck attrs]
      pure a
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      isPoisoned <- getIsPoisoned iid
      if isPoisoned
        then push $ assignDamageAndHorror iid attrs 1 1
        else do
          poisoned <- getSetAsidePoisoned
          push $ CreateWeaknessInThreatArea poisoned iid
      pure a
    _ -> ExpeditionIntoTheWild <$> runMessage msg attrs
