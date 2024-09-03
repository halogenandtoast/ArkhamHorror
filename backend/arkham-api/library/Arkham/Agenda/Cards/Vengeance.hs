module Arkham.Agenda.Cards.Vengeance (Vengeance (..), vengeance) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner hiding (InvestigatorDefeated)
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Prelude
import Data.List (cycle)

newtype Vengeance = Vengeance AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vengeance :: AgendaCard Vengeance
vengeance = agendaWith (7, A) Vengeance Cards.vengeance (Static 0) (doomThresholdL .~ Nothing)

instance HasAbilities Vengeance where
  getAbilities (Vengeance a) =
    [ mkAbility a 1 $ forced $ MythosStep AfterCheckDoomThreshold
    , restrictedAbility a 2 (notExists UneliminatedInvestigator)
        $ forced
        $ InvestigatorDefeated #when ByAny Anyone
    ]

instance RunMessage Vengeance where
  runMessage msg a@(Vengeance attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      push R1
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      doom <- getDoomCount
      investigators <- getInvestigatorPlayers
      pushAll
        $ zipWith
          ($)
          ( replicate doom \(investigator, player) ->
              chooseOne
                player
                [TargetLabel EncounterDeckTarget [drawEncounterCards investigator attrs 1]]
          )
          (cycle investigators)
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      push $ AdvanceAgenda (toId attrs)
      pure a
    _ -> Vengeance <$> runMessage msg attrs
