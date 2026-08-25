module Arkham.Homebrew.DarkMatter.Agendas.ANightmare (aNightmare) where

import Arkham.Agenda.Import.Lifted
import Arkham.Enemy.CardDefs.ThePathToCarcosa.CurtainCall qualified as CurtainCall
import Arkham.Helpers.Log (getHasRecord)
import Arkham.Helpers.Query (getLead)
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.DarkMatter.Key
import Arkham.Homebrew.DarkMatter.MachineInYellow
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype ANightmare = ANightmare AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aNightmare :: AgendaCard ANightmare
aNightmare = agenda (2, A) ANightmare Cards.aNightmare (Static 5)

instance HasAbilities ANightmare where
  getAbilities (ANightmare a) = [memoriesInsteadOfHorror a]

instance RunMessage ANightmare where
  runMessage msg a@(ANightmare attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 ws _ -> do
      crossOffMemoriesInsteadOfHorror iid ws
      pure a
    {- Agenda 2b:

    "Check the Campaign Log. If you fully restored the sanity of K2-PS187:
    Search the collection for The Boogeyman (Virtual Nightmare) and spawn it at
    the location furthest from all investigators.
    Otherwise: Search the collection for Royal Emissary (Messenger from
    Aldebaran) and spawn it at the location furthest from all investigators."

    Neither enemy is gathered by this scenario — Curtain Call contributes only
    its locations — so both come from the collection. -}
    AdvanceAgenda (isSide B attrs -> True) -> do
      restoredK2 <- getHasRecord YouFullyRestoredTheSanityOfK2PS187
      nightmare <-
        fetchCard $ if restoredK2 then Enemies.theBOOGEYMAN else CurtainCall.royalEmissary
      locations <- select $ FarthestLocationFromAll Anywhere
      lead <- getLead
      chooseOrRunOneM lead $ targets locations (createEnemyAt_ nightmare)
      advanceAgendaDeck attrs
      pure a
    _ -> ANightmare <$> liftRunMessage msg attrs
