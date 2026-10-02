module Arkham.Homebrew.CircusExMortis.Agendas.FadingSunlightVII (fadingSunlightVII) where

import Arkham.Ability
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.CircusExMortis.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Enemies
import Arkham.Matcher

newtype FadingSunlightVII = FadingSunlightVII AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- Back ("Darkness Falls") is only "(→R1)".
fadingSunlightVII :: AgendaCard FadingSunlightVII
fadingSunlightVII = agenda (1, A) FadingSunlightVII Cards.fadingSunlightVII (Static 17)

-- "Investigators cannot spend clues unless there is an enemy at their location."
instance HasModifiersFor FadingSunlightVII where
  getModifiersFor (FadingSunlightVII a) = when (onSide A a) do
    modifySelect a (InvestigatorAt LocationWithoutEnemies) [CannotSpendClues]

instance HasAbilities FadingSunlightVII where
  getAbilities (FadingSunlightVII a) =
    [ restricted
        a
        1
        (SetAsideCardExists $ cardIs Enemies.devoteeOfTheThousand)
        freeTrigger_
    ]

instance RunMessage FadingSunlightVII where
  runMessage msg a@(FadingSunlightVII attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      push R1
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withLocationOf iid $ createSetAsideEnemy_ Enemies.devoteeOfTheThousand
      pure a
    _ -> FadingSunlightVII <$> liftRunMessage msg attrs
