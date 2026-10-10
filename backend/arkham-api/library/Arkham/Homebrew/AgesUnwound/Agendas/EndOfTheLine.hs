module Arkham.Homebrew.AgesUnwound.Agendas.EndOfTheLine (endOfTheLine) where

import Arkham.Ability
import Arkham.Agenda.Import.Lifted
import Arkham.Effect.Builder
import Arkham.Effect.Window
import Arkham.Helpers.Modifiers (ModifierType (AdditionalActions))
import Arkham.Homebrew.AgesUnwound.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.AgesUnwound.Helpers (getStandardActions)
import Arkham.Homebrew.AgesUnwound.Scenarios.AWorldTornDown.Helpers
import Arkham.I18n
import Arkham.Matcher

newtype EndOfTheLine = EndOfTheLine AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

endOfTheLine :: AgendaCard EndOfTheLine
endOfTheLine = agenda (3, A) EndOfTheLine Cards.endOfTheLine (Static 4)

{- | "Forced - At the end of your turn, if you have any standard actions
remaining: You gain actions for the next round equal to the number you have
remaining (to a maximum of 3)."

Plus the @[action]@ agenda 1b hands to whichever agenda is current. The "if you
have any standard actions remaining" check is in the handler for the same reason
as on agenda 2a -- see 'Arkham.Homebrew.AgesUnwound.Agendas.TimeRunningOut'.
-}
instance HasAbilities EndOfTheLine where
  getAbilities (EndOfTheLine a) =
    [ forcedAbility a 1 $ TurnEnds #when You
    , grantedAidFromAfarAbility a 2
    ]

instance RunMessage EndOfTheLine where
  runMessage msg a@(EndOfTheLine attrs) = runQueueT $ scenarioI18n $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      standard <- getStandardActions iid
      let gained = min 3 standard
      when (gained > 0) do
        {- Same shape as agenda 2a's: 'EffectEndOfNextTurnWindow' is the window
        that lasts from now to the end of this investigator's next turn, which is
        what "for the next round" means once @Do BeginRound@ has thrown away the
        old action count. -}
        effectWithSource (attrs.ability 1) iid do
          removeOn $ EffectEndOfNextTurnWindow iid
          apply
            $ AdditionalActions (ikey' "endOfTheLine.additionalActions") (attrs.ability 1) gained
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      drawAidFromAfar iid
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      -- "Each remaining investigator is defeated."
      eachInvestigator (investigatorDefeated attrs)
      pure a
    _ -> EndOfTheLine <$> liftRunMessage msg attrs
