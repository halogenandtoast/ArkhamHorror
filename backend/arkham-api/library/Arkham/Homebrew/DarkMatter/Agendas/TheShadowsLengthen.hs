module Arkham.Homebrew.DarkMatter.Agendas.TheShadowsLengthen (theShadowsLengthen) where

import Arkham.Agenda.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Cards

newtype TheShadowsLengthen = TheShadowsLengthen AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | "There is no doom threshold (keep adding doom to this agenda)." The agenda
accumulates doom and is advanced by the scenario rather than by threshold.
-}
theShadowsLengthen :: AgendaCard TheShadowsLengthen
theShadowsLengthen =
  agendaWith (1, A) TheShadowsLengthen Cards.theShadowsLengthen (Static 0)
    $ doomThresholdL
    .~ Nothing

instance RunMessage TheShadowsLengthen where
  runMessage msg a@(TheShadowsLengthen attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> TheShadowsLengthen <$> liftRunMessage msg attrs
