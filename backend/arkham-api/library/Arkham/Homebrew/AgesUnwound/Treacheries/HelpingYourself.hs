module Arkham.Homebrew.AgesUnwound.Treacheries.HelpingYourself (helpingYourself) where

import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgesUnwound.Missions.Helpers (completeTaskId)
import Arkham.Token qualified as Token
import Arkham.Treachery.Import.Lifted
import Arkham.Treachery.Types (treacheryResources)

newtype HelpingYourself = HelpingYourself TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Enters play at setup "next to the act deck with X resources on it", X being
the @StrangeAssistance@ tally -- the scenario's job, so there is no
__Revelation__ here to do it.
-}
helpingYourself :: TreacheryCard HelpingYourself
helpingYourself = treachery HelpingYourself Cards.helpingYourself

{- | "__Task__ - Remove all resources from Helping Yourself."

Arkham, Massachusetts prints the only way to take one off. There is no window
for a token /leaving/ a card -- @PlacedToken@ has no counterpart -- so the
objective is checked on the message itself, after the base handler has applied
the subtraction. A 'SilentForcedAbility' on @RoundEnds@ would also work but
would leave the Task standing (and counted by the scenario's [skull] token) for
the rest of the round after it was already met.
-}
instance RunMessage HelpingYourself where
  runMessage msg (HelpingYourself attrs) = runQueueT $ case msg of
    RemoveTokens _ (isTarget attrs -> True) Token.Resource _ -> checkObjective
    MoveTokens _ (isSource attrs -> True) _ Token.Resource _ -> checkObjective
    _ -> HelpingYourself <$> liftRunMessage msg attrs
   where
    checkObjective = do
      attrs' <- liftRunMessage msg attrs
      when (treacheryResources attrs' == 0) $ completeTaskId attrs'
      pure (HelpingYourself attrs')
