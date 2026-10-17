module Arkham.Homebrew.AgesUnwound.Treacheries.NotWelcomeHere (notWelcomeHere) where

import Arkham.Helpers.Modifiers (ModifierType (CannotMove))
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.TheMyriadGentleman.Helpers
import Arkham.Homebrew.AgesUnwound.Traits (pattern Myriad)
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move (moveTowardsMatching)
import Arkham.Treachery.Import.Lifted

newtype NotWelcomeHere = NotWelcomeHere TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

notWelcomeHere :: TreacheryCard NotWelcomeHere
notWelcomeHere = treachery NotWelcomeHere Cards.notWelcomeHere

{- | "Peril. Revelation - Choose an investigator. Move each [[Myriad]] enemy in
play once towards the chosen investigator. Spawn copies of The Myriad Gentleman
engaged with that investigator until they are engaged with at least 3 [[Myriad]]
enemies."

The top-up is counted after the moves land, so it has to wait for the queue --
hence the 'Msg.ForInvestigator' hand-off rather than reading the count up front.
Swarm cards are collapsed out of the move loop: 'enemyEngagedWith' and plain
enemy selects return them alongside their host, and a swarm card's move message
is redirected to that host anyway.
-}
instance RunMessage NotWelcomeHere where
  runMessage msg t@(NotWelcomeHere attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      iids <- allInvestigators
      chooseOrRunOneM iid $ targets iids \chosen -> do
        {- An enemy that cannot move is left where it is. Without the guard the
        move machinery still offers the lead a path to choose for it, and then
        refuses the move. -}
        myriad <- select $ EnemyWithTrait Myriad <> not_ IsSwarm <> not_ (EnemyWithModifier CannotMove)
        for_ myriad \eid ->
          moveTowardsMatching attrs eid (locationWithInvestigator chosen)
        push $ Msg.ForInvestigator chosen (Msg.Do msg)
      pure t
    Msg.ForInvestigator chosen (Msg.Do (Revelation _ (isSource attrs -> True))) -> do
      engaged <- selectCount $ EnemyWithTrait Myriad <> not_ IsSwarm <> enemyEngagedWith chosen
      spawnMyriadCopiesEngagedWith chosen (max 0 (3 - engaged))
      pure t
    _ -> NotWelcomeHere <$> liftRunMessage msg attrs
