module Arkham.Agenda.Cards.TheDoomOfArkham (theDoomOfArkham) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher

newtype TheDoomOfArkham = TheDoomOfArkham AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theDoomOfArkham :: AgendaCard TheDoomOfArkham
theDoomOfArkham = agenda (1, A) TheDoomOfArkham Cards.theDoomOfArkham (Static 3)

instance HasAbilities TheDoomOfArkham where
  getAbilities (TheDoomOfArkham a) =
    [ mkAbility a 1 $ forced $ PhaseEnds #when #enemy
    , restricted a 2 (exists $ InvestigatorAt FullyFloodedLocation)
        $ forced
        $ RoundEnds #when
    ]

instance RunMessage TheDoomOfArkham where
  runMessage msg a@(TheDoomOfArkham attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      -- [Forced] When the enemy phase ends: increase the flood level of
      -- Cthulhu's location, then draw the top card of the Cthulhu deck.
      selectEach (LocationWithEnemy $ enemyIs Enemies.cthulhuAncientEvil) (push . IncreaseFloodLevel)
      -- TODO: draw the top card of the Cthulhu deck (no engine support yet)
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      -- [Forced] When the round ends: each investigator at a fully flooded
      -- location takes 1 damage and 1 horror.
      investigators <- select $ InvestigatorAt FullyFloodedLocation
      for_ investigators \iid -> assignDamageAndHorror iid (attrs.ability 2) 1 1
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      -- Place clues on each revealed location without Victory X up to its clue value.
      locations <- select $ not_ LocationWithVictory
      for_ locations $ placeCluesUpToClueValue attrs
      -- TODO: Ruined-location swap, the 7-Ruined kill, and shuffling the Star
      -- Spawn back into the encounter deck (no engine support yet).
      advanceAgendaDeck attrs
      pure a
    _ -> TheDoomOfArkham <$> liftRunMessage msg attrs
