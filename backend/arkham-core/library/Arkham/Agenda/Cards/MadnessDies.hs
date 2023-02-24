module Arkham.Agenda.Cards.MadnessDies
  ( MadnessDies(..)
  , madnessDies
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Campaigns.ThePathToCarcosa.Helpers
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message
import Arkham.Resolution

newtype MadnessDies = MadnessDies AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

madnessDies :: AgendaCard MadnessDies
madnessDies = agenda (3, A) MadnessDies Cards.madnessDies (Static 9)

instance HasModifiersFor MadnessDies where
  getModifiersFor (EnemyTarget eid) (MadnessDies a) = do
    isHastur <- eid `isMatch` EnemyWithTitle "Hastur"
    pure $ toModifiers a [ EnemyFight 2 | isHastur ]
  getModifiersFor _ _ = pure []

instance RunMessage MadnessDies where
  runMessage msg a@(MadnessDies attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      conviction <- getConviction
      doubt <- getDoubt
      case compare conviction doubt of
        GT -> push $ ScenarioResolution $ Resolution 4
        EQ -> push $ ScenarioResolution $ Resolution 4
        LT -> push $ ScenarioResolution $ Resolution 5
      pure a
    _ -> MadnessDies <$> runMessage msg attrs
