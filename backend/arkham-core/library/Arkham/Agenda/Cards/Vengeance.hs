module Arkham.Agenda.Cards.Vengeance
  ( Vengeance(..)
  , vengeance
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message hiding ( InvestigatorDefeated )
import Arkham.Resolution
import Arkham.Target
import Arkham.Timing qualified as Timing
import Data.List ( cycle )

newtype Vengeance = Vengeance AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vengeance :: AgendaCard Vengeance
vengeance = agendaWith
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
      $ InvestigatorDefeated Timing.When AnySource ByAny Anyone
    ]

instance RunMessage Vengeance where
  runMessage msg a@(Vengeance attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      push $ ScenarioResolution $ Resolution 1
      pure a
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      doom <- getDoomCount
      iids <- getInvestigatorIds
      pushAll $ zipWith
        ($)
        (replicate
          doom
          (\i -> chooseOne
            i
            [TargetLabel EncounterDeckTarget [InvestigatorDrawEncounterCard i]]
          )
        )
        (cycle iids)
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 _ _ -> do
      push $ AdvanceAgenda (toId attrs)
      pure a
    _ -> Vengeance <$> runMessage msg attrs
