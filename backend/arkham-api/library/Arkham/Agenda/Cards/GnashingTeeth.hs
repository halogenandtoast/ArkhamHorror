module Arkham.Agenda.Cards.GnashingTeeth (gnashingTeeth) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Location
import Arkham.Helpers.Log (scenarioCount)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Query (getLead)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.ScenarioLogKey
import Arkham.Scenarios.DeadHeat.Helpers
import Arkham.Spawn
import Arkham.Trait (Trait (Ghoul, Risen))

newtype GnashingTeeth = GnashingTeeth AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gnashingTeeth :: AgendaCard GnashingTeeth
gnashingTeeth = agenda (1, A) GnashingTeeth Cards.gnashingTeeth (Static 8)

instance HasModifiersFor GnashingTeeth where
  getModifiersFor (GnashingTeeth a) = do
    modifySelect a AnyEnemy [ForceSpawn SpawnAtRandomLocation]

instance RunMessage GnashingTeeth where
  runMessage msg a@(GnashingTeeth attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      slain <- scenarioCount CiviliansSlain
      pc <- perPlayer 1
      let n = slain `div` pc
      lead <- getLead
      repeated n $ findAndDrawEncounterCard lead $ #enemy <> mapOneOf CardWithTrait [Risen, Ghoul]
      shuffleEncounterDiscardBackIn
      eachInvestigator \iid -> do
        chooseOneM iid $ scenarioI18n do
          labeled' "gnashingTeeth.heal" $ selectEach EliteEnemy $ healDamageOn attrs 1
          labeled' "gnashingTeeth.slain" $ withLocationOf iid slayCivilian
      advanceAgendaDeck attrs
      pure a
    _ -> GnashingTeeth <$> liftRunMessage msg attrs
