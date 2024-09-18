module Arkham.Agenda.Cards.DecrepitDecay (DecrepitDecay (..), decrepitDecay) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted hiding (EnemyDefeated)
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Campaigns.TheInnsmouthConspiracy.Memory
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query
import Arkham.Helpers.Window
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Scenarios.TheVanishingOfElinaHarper.Helpers

newtype DecrepitDecay = DecrepitDecay AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

decrepitDecay :: AgendaCard DecrepitDecay
decrepitDecay = agenda (1, A) DecrepitDecay Cards.decrepitDecay (Static 6)

instance HasAbilities DecrepitDecay where
  getAbilities (DecrepitDecay a) =
    [forcedAbility a 1 $ EnemyDefeated #when You (BySource $ SourceOwnedBy You) notKidnapper]

instance RunMessage DecrepitDecay where
  runMessage msg a@(DecrepitDecay attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      shuffleSetAsideIntoEncounterDeck [Enemies.wingedOneFogOverInnsmouth, Enemies.huntingNightgaunt]
      shuffleEncounterDiscardBackIn
      whenRecoveredMemory ADecisionToStickTogether do
        lead <- getLead
        thomasDawson <- createAsset =<< getSetAsideCard Assets.thomasDawsonSoldierInANewWar
        gameModifier attrs thomasDawson (DoNotTakeUpSlot #ally)
        chooseFromM lead UneliminatedInvestigator (`takeControlOfAsset` thomasDawson)
      advanceAgendaDeck attrs
      pure a
    UseCardAbility iid (isSource attrs -> True) 1 (defeatedEnemy -> enemy) _ -> do
      moveAllTokens (attrs.ability 1) enemy iid #clue
      outForBlood enemy
      pure a
    _ -> DecrepitDecay <$> liftRunMessage msg attrs
