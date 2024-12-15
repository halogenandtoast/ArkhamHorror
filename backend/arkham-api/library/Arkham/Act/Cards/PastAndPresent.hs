module Arkham.Act.Cards.PastAndPresent (
  PastAndPresent (..),
  pastAndPresent,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Trait

newtype PastAndPresent = PastAndPresent ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pastAndPresent :: ActCard PastAndPresent
pastAndPresent = act (2, A) PastAndPresent Cards.pastAndPresent Nothing

instance HasAbilities PastAndPresent where
  getAbilities (PastAndPresent a) =
    [ restrictedAbility
      a
      1
      ( LocationCount 6
          $ LocationWithTrait Tenochtitlan
          <> LocationWithoutClues
      )
      $ Objective
      $ ForcedAbility AnyWindow
    | onSide A a
    ]

instance RunMessage PastAndPresent where
  runMessage msg a@(PastAndPresent attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ AdvanceAct (toId attrs) (toSource attrs) AdvancedWithOther
      pure a
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      tenochtitlanLocations <-
        select $ LocationWithTrait Tenochtitlan <> LocationWithoutClues
      iids <- getInvestigators
      timeCollapsing <- getSetAsideCard Agendas.timeCollapsing
      theReturnTrip <- getSetAsideCard Acts.theReturnTrip

      pushAll
        $ map (AddToVictory . LocationTarget) tenochtitlanLocations
        <> [NextAdvanceActStep (toId attrs) 1]
        <> map (InvestigatorDiscardAllClues (toAbilitySource attrs 1)) iids
        <> [NextAdvanceActStep (toId attrs) 2]
        <> [ SetCurrentAgendaDeck 1 [timeCollapsing]
           , SetCurrentActDeck 1 [theReturnTrip]
           ]
      pure a
    NextAdvanceActStep aid 1 | aid == toId attrs && onSide B attrs -> do
      presentDayLocations <- select $ LocationWithTrait PresentDay
      (leadInvestigatorId, lead) <- getLeadInvestigatorPlayer
      push
        $ chooseOneAtATime
          lead
          [ targetLabel
            lid
            [ HandleTargetChoice
                leadInvestigatorId
                (toSource attrs)
                (LocationTarget lid)
            ]
          | lid <- presentDayLocations
          ]
      pure a
    HandleTargetChoice iid (isSource attrs -> True) (LocationTarget lid) -> do
      locationSymbol <- field LocationPrintedSymbol lid
      replacements <-
        filter ((== Just locationSymbol) . cdLocationRevealedSymbol . toCardDef)
          <$> getExplorationDeck
      player <- getPlayer iid
      pushAll
        [ FocusCards replacements
        , chooseOrRunOne
            player
            [ targetLabel
              (toCardId replacement)
              [ RemoveCardFromScenarioDeck ExplorationDeck replacement
              , ReplaceLocation lid replacement DefaultReplace
              ]
            | replacement <- replacements
            ]
        , UnfocusCards
        ]
      pure a
    NextAdvanceActStep aid 2 | aid == toId attrs && onSide B attrs -> do
      padma <- getSetAsideCard Enemies.padmaAmrita
      temploMayor <- selectJust $ LocationWithTitle "Templo Mayor"
      pushM $ createEnemyAt_ padma temploMayor Nothing
      pure a
    _ -> PastAndPresent <$> runMessage msg attrs
