module Arkham.Agenda.Cards.ExpeditionIntoTheWild
  ( ExpeditionIntoTheWild(..)
  , expeditionIntoTheWild
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Deck qualified as Deck
import Arkham.EncounterSet
import Arkham.GameValue
import Arkham.Helpers.Investigator
import Arkham.Helpers.Location
import Arkham.Matcher
import Arkham.Message
import Arkham.Scenario.Deck
import Arkham.SkillType

newtype ExpeditionIntoTheWild = ExpeditionIntoTheWild AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

expeditionIntoTheWild :: AgendaCard ExpeditionIntoTheWild
expeditionIntoTheWild =
  agenda (1, A) ExpeditionIntoTheWild Cards.expeditionIntoTheWild (Static 6)

instance HasAbilities ExpeditionIntoTheWild where
  getAbilities (ExpeditionIntoTheWild a) =
    [ restrictedAbility a 1 (ScenarioDeckWithCard ExplorationDeck)
        $ ActionAbility (Just Action.Explore)
        $ ActionCost 1
    ]

instance RunMessage ExpeditionIntoTheWild where
  runMessage msg a@(ExpeditionIntoTheWild attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      locationSymbols <- toConnections =<< getJustLocation iid
      push $ Explore
        iid
        source
        (CardWithOneOf $ map CardWithPrintedLocationSymbol locationSymbols)
      pure a
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      setAsideAgentsOfYig <- getSetAsideEncounterSet AgentsOfYig
      iids <- getInvestigatorIds
      pushAll
        $ [ ShuffleEncounterDiscardBackIn
          , ShuffleCardsIntoDeck Deck.EncounterDeck setAsideAgentsOfYig
          ]
        <> [ beginSkillTest
               iid
               (toSource attrs)
               (InvestigatorTarget iid)
               SkillWillpower
               3
           | iid <- iids
           ]
        <> [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
      pure a
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> do
        isPoisoned <- getIsPoisoned iid
        if isPoisoned
          then push $ InvestigatorAssignDamage iid source DamageAny 1 1
          else do
            poisoned <- getSetAsidePoisoned
            push $ CreateWeaknessInThreatArea poisoned iid
        pure a
    _ -> ExpeditionIntoTheWild <$> runMessage msg attrs
