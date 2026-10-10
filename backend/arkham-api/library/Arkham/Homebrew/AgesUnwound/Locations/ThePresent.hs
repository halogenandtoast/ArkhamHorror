module Arkham.Homebrew.AgesUnwound.Locations.ThePresent (thePresent) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (FewerActions))
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (revealedL)
import Arkham.Matcher

newtype ThePresent = ThePresent LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePresent :: LocationCard ThePresent
thePresent =
  symbolLabel $ locationWith ThePresent Cards.thePresent 3 (PerPlayer 2) $ revealedL .~ True

{- | "[free]: If you have yet to take your turn this round: Take an action as if
it were your turn. This action counts toward the number of actions you can take
each turn. (Limit once per round.)"

'takeActionAsIfTurn' grants the action that makes the out-of-turn window
affordable, and "counts toward the number of actions you can take each turn" is
the 'FewerActions' modifier queued for the turn that has not started yet:
@Begin InvestigationPhase@ re-derives an investigator's actions and throws away
accumulated 'Arkham.Message.GainActions', so without it the borrowed action would
be free.
-}
instance HasAbilities ThePresent where
  getAbilities (ThePresent a) =
    extendRevealed1 a
      $ groupLimit PerRound
      $ restricted a 1 (Here <> youExist YetToTakeTurn) (FastAbility Free)

instance RunMessage ThePresent where
  runMessage msg l@(ThePresent attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      nextTurnModifier iid (attrs.ability 1) iid (FewerActions 1)
      takeActionAsIfTurn iid (attrs.ability 1)
      pure l
    _ -> ThePresent <$> liftRunMessage msg attrs
