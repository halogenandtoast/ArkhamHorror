module Arkham.Agenda.Cards.TerrorDescends (terrorDescends) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Matcher

newtype TerrorDescends = TerrorDescends AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

terrorDescends :: AgendaCard TerrorDescends
terrorDescends = agenda (2, A) TerrorDescends Cards.terrorDescends (Static 8)

instance HasAbilities TerrorDescends where
  getAbilities (TerrorDescends a) =
    [ mkAbility a 1 $ forced $ Moves #after (You <> InvestigatorWithAnyClues) AnySource Anywhere Anywhere
    ]

instance RunMessage TerrorDescends where
  runMessage msg a@(TerrorDescends attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      anyResigned <- selectAny ResignedInvestigator
      push $ if anyResigned then R1 else R2
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discardAllClues (attrs.ability 1) iid
      pure a
    _ -> TerrorDescends <$> liftRunMessage msg attrs
