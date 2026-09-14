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
    -- The back is a joke with no in-game way to reach it, so the Konami code is
    -- the only thing that flips it. Agenda 2a is reached from act 2b instead.
    KonamiCode _ -> do
      advanceAgenda attrs
      pure a
    {- Agenda 1b:

    "You aren't supposed to see this. (Hint: 'Time is a flat circle. Everything
    we have done or will do we will do over and over and over again — forever.')"

    No mechanical effect; flipping back is all it does. -}
    AdvanceAgenda (isSide B attrs -> True) -> do
      revertAgenda attrs
      pure a
    _ -> TheShadowsLengthen <$> liftRunMessage msg attrs
