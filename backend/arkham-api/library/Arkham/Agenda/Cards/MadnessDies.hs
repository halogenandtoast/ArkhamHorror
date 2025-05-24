module Arkham.Agenda.Cards.MadnessDies (madnessDies) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.ThePathToCarcosa.Helpers
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype MadnessDies = MadnessDies AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

madnessDies :: AgendaCard MadnessDies
madnessDies = agenda (3, A) MadnessDies Cards.madnessDies (Static 9)

instance HasModifiersFor MadnessDies where
  getModifiersFor (MadnessDies a) = modifySelect a (EnemyWithTitle "Hastur") [EnemyFight 2]

instance RunMessage MadnessDies where
  runMessage msg a@(MadnessDies attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      conviction <- getConviction
      doubt <- getDoubt
      push $ if conviction >= doubt then R4 else R5
      pure a
    _ -> MadnessDies <$> liftRunMessage msg attrs
