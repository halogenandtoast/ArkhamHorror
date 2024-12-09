module Arkham.Agenda.Cards.ATrailOfTwists (
  ATrailOfTwists (..),
  aTrailOfTwists,
) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Game.Helpers (perPlayer)
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype ATrailOfTwists = ATrailOfTwists AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aTrailOfTwists :: AgendaCard ATrailOfTwists
aTrailOfTwists =
  agendaWith (2, A) ATrailOfTwists Cards.aTrailOfTwists (Static 9)
    $ removeDoomMatchersL
    %~ (\m -> m {removeDoomLocations = Nowhere})

instance HasModifiersFor ATrailOfTwists where
  getModifiersFor (ATrailOfTwists attrs) = do
    n <- perPlayer 2
    modifySelf attrs [DoomThresholdModifier n]

instance RunMessage ATrailOfTwists where
  runMessage msg a@(ATrailOfTwists attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      shuffleEncounterDiscardBackIn
      advanceAgendaDeck attrs
      pure a
    _ -> ATrailOfTwists <$> liftRunMessage msg attrs
