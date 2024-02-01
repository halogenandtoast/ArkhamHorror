module Arkham.Agenda.Cards.Vengeance (
  Vengeance (..),
  vengeance,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner hiding (InvestigatorDefeated)
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Resolution
import Arkham.Timing qualified as Timing
import Data.List (cycle)

newtype Vengeance = Vengeance AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

vengeance :: AgendaCard Vengeance
vengeance =
  agendaWith
    (7, A)
    Vengeance
    Cards.vengeance
    (Static 0)
    (doomThresholdL .~ Nothing)

instance HasAbilities Vengeance where
  getAbilities (Vengeance a) =
    [ mkAbility a 1 $ ForcedAbility $ MythosStep AfterCheckDoomThreshold
    , restrictedAbility
        a
        2
        (Negate $ InvestigatorExists UneliminatedInvestigator)
        $ ForcedAbility
        $ InvestigatorDefeated Timing.When ByAny Anyone
    ]

instance RunMessage Vengeance where
  runMessage msg a@(Vengeance attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      push $ ScenarioResolution $ Resolution 1
      pure a
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      doom <- getDoomCount
      investigators <- getInvestigatorPlayers
      pushAll
        $ zipWith
          ($)
          ( replicate doom \(investigator, player) ->
              chooseOne
                player
                [TargetLabel EncounterDeckTarget [InvestigatorDrawEncounterCard investigator]]
          )
          (cycle investigators)
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 _ _ -> do
      push $ AdvanceAgenda (toId attrs)
      pure a
    _ -> Vengeance <$> runMessage msg attrs
