module Arkham.Act.Cards.DescentIntoTheMines (descentIntoTheMines) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Agenda.Types (Field (AgendaDoom))
import Arkham.Asset.Cards qualified as Assets
import Arkham.Helpers.Agenda
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message (TokenLoss (..))
import Arkham.Projection
import Arkham.Token
import Arkham.Trait (Trait (Cave))

newtype DescentIntoTheMines = DescentIntoTheMines ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

descentIntoTheMines :: ActCard DescentIntoTheMines
descentIntoTheMines = act (1, A) DescentIntoTheMines Cards.descentIntoTheMines Nothing

instance HasAbilities DescentIntoTheMines where
  getAbilities (DescentIntoTheMines a) =
    [ restricted a 1 (EachUndefeatedInvestigator $ at_ $ locationIs Locations.controlStation)
        $ Objective
        $ forced (RoundEnds #when)
    ]

instance RunMessage DescentIntoTheMines where
  runMessage msg a@(DescentIntoTheMines attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      selectEach (location_ $ withTrait Cave) removeLocation
      eachInvestigator \iid -> do
        push $ LoseTokens iid (toSource attrs) Clue (AllLostBut 2)

      n <- getCurrentAgendaStep
      when (n == 1) do
        doom <- field AgendaDoom =<< selectJust AnyAgenda
        advanceToAgendaA attrs Agendas.dangerousRide
        placeDoomOnAgenda doom

      scenarioSpecific_ "theCaveIn"
      advanceActDeck attrs
      selectEach (assetIs Assets.drRosaMarquezBestInHerField) \asset -> do
        clearAbilityUse $ AbilityRef (toSource asset) 1
      pure a
    _ -> DescentIntoTheMines <$> liftRunMessage msg attrs
