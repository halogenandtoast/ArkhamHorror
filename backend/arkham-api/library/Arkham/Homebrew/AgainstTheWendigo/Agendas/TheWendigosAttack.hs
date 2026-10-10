{- | The Wendigo's Attack, printed on the back of agenda 3, The Wendigo Hunts
You.

It is its own agenda here rather than that card's @b@ side, because what it does
is /stay/: it replaces the current agenda and the scenario then runs until The
Wendigo is defeated. It prints no doom value, so it never advances.
-}
module Arkham.Homebrew.AgainstTheWendigo.Agendas.TheWendigosAttack (theWendigosAttack) where

import Arkham.Ability
import Arkham.Agenda.Import.Lifted
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Enemies qualified as Enemies
import Arkham.Matcher

newtype TheWendigosAttack = TheWendigosAttack AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theWendigosAttack :: AgendaCard TheWendigosAttack
theWendigosAttack =
  agendaWith (4, A) TheWendigosAttack Cards.theWendigosAttack (Static 0) (doomThresholdL .~ Nothing)

{- | "Objective - If The Wendigo is defeated: (-> R1)."

The Civilized locations keep their "{action}: Resign" here too, which they read
off the agenda step rather than off this card; see 'civilizedResign'.
-}
instance HasAbilities TheWendigosAttack where
  getAbilities (TheWendigosAttack a) =
    [restricted a 1 (notExists $ enemyIs Enemies.theWendigo) $ Objective freeTrigger_]

instance RunMessage TheWendigosAttack where
  runMessage msg a@(TheWendigosAttack attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push R1
      pure a
    _ -> TheWendigosAttack <$> liftRunMessage msg attrs
