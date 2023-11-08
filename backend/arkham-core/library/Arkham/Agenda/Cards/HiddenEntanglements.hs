module Arkham.Agenda.Cards.HiddenEntanglements (
  HiddenEntanglements (..),
  hiddenEntanglements,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher hiding (WindowMatcher (InvestigatorDefeated))

newtype HiddenEntanglements = HiddenEntanglements AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hiddenEntanglements :: AgendaCard HiddenEntanglements
hiddenEntanglements =
  agenda (3, A) HiddenEntanglements Cards.hiddenEntanglements (Static 6)

instance HasAbilities HiddenEntanglements where
  getAbilities (HiddenEntanglements attrs) =
    [mkAbility attrs 1 $ ActionAbility [Action.Resign] (ActionCost 1)]

instance RunMessage HiddenEntanglements where
  runMessage msg a@(HiddenEntanglements attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push (Resign iid)
      HiddenEntanglements <$> runMessage msg attrs
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      iids <- selectList UneliminatedInvestigator
      a
        <$ pushAll
          ( concatMap
              ( \iid ->
                  [SufferTrauma iid 0 1, InvestigatorDefeated (toSource attrs) iid]
              )
              iids
          )
    _ -> HiddenEntanglements <$> runMessage msg attrs
