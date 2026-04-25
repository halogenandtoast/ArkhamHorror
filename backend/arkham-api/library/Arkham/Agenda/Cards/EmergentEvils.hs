module Arkham.Agenda.Cards.EmergentEvils (EmergentEvils (..), emergentEvils) where

-- Constructor is only exported for testing purposes

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype EmergentEvils = EmergentEvils AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

emergentEvils :: AgendaCard EmergentEvils
emergentEvils = agenda (2, A) EmergentEvils Cards.emergentEvils (Static 8)

instance HasAbilities EmergentEvils where
  getAbilities (EmergentEvils a) =
    [ withTooltip
        "_Resign_. You return to compare notes with Dr. Armitage on your friend's whereabouts."
        $ restricted a 1 NoRestriction
        $ ActionAbility #resign Nothing
        $ ActionCost 1
    | onSide A a
    ]

instance RunMessage EmergentEvils where
  runMessage msg a@(EmergentEvils attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ Resign iid
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid -> push $ Resign iid
      advanceAgendaDeck attrs
      pure a
    _ -> EmergentEvils <$> liftRunMessage msg attrs
