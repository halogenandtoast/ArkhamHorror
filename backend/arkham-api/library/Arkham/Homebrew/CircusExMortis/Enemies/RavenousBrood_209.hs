module Arkham.Homebrew.CircusExMortis.Enemies.RavenousBrood_209 (
  ravenousBrood_209,
  broodAbilities,
  setAsideInsteadOfLeavingPlay,
) where

import Arkham.Ability
import Arkham.Card
import Arkham.Classes.HasGame (HasGame)
import Arkham.Classes.HasQueue (HasQueue)
import Arkham.Enemy.Import.Lifted
import Arkham.GameEnv (getCard)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Cards
import Arkham.Matcher
import Arkham.Message (ReplaceStrategy (..))
import Arkham.Message qualified as Msg
import Arkham.Queue (QueueT)

newtype RavenousBrood_209 = RavenousBrood_209 EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | The hunter/retaliate face. Both faces are enemies, so each is its own card def
pointing at the other (the Cthulhu / The Organist shape); the alert face lives in
"Arkham.Homebrew.CircusExMortis.Enemies.RavenousBrood_209b".

The printed health on this face is @X@ ('healthX'), and the card's only definition of X is
the line "X is equal to the number of players" -- so this face's health /is/ the player
count. Nothing else on this face is variable: fight 3, evade 2 and the damage/horror are
all printed numbers. (The other face spends the same X on fight instead: it prints health
2 and "gets +X fight".)
-}
ravenousBrood_209 :: EnemyCard RavenousBrood_209
ravenousBrood_209 = enemy RavenousBrood_209 Cards.ravenousBrood_209

instance HasModifiersFor RavenousBrood_209 where
  getModifiersFor (RavenousBrood_209 a) = do
    n <- getPlayerCount
    modifySelf a [HealthModifier n, CannotHaveAttachments]

instance HasAbilities RavenousBrood_209 where
  getAbilities (RavenousBrood_209 a) = broodAbilities a

instance RunMessage RavenousBrood_209 where
  runMessage msg e@(RavenousBrood_209 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 ->
      RavenousBrood_209 <$> setAsideInsteadOfLeavingPlay attrs
    -- Each face is its own card, so a flip is a swap to the other one. Nothing in the
    -- engine does this for an enemy: `Flip` has no handler in 'Enemy.Runner'.
    Flip _ _ (isTarget attrs -> True) -> do
      push $ ReplaceEnemy attrs.id (lookupCard Cards.ravenousBrood_209b attrs.cardId) Swap
      pure e
    _ -> RavenousBrood_209 <$> liftRunMessage msg attrs

-- | "__Forced__ - If Ravenous Brood would leave play: Set it aside, out of play."
broodAbilities :: EnemyAttrs -> [Ability]
broodAbilities a =
  extend1 a $ restricted a 1 (thisExists a AnyEnemy) $ forced $ EnemyLeavesPlay #when (be a)

{- | Shub-Niggurath's end-of-round spawn draws from the set-aside pile, so a Brood that
leaves play has to go back to it rather than to the encounter discard. Same shape as
Devotee of the Thousand: the removal itself stands, only its destination is replaced, and
dropping the queued discard is what keeps the card out of the encounter discard so
'SetCardAside' is the only place it lands.

The returned attrs are stripped of their tokens. The entity outlives the card here -- it
stays in 'RemovedZone' while the card sits set aside -- so damage it took on its way out
would otherwise still be showing when it is spawned again.
-}
setAsideInsteadOfLeavingPlay
  :: (HasGame m, HasQueue Message m) => EnemyAttrs -> QueueT Message m EnemyAttrs
setAsideInsteadOfLeavingPlay attrs = do
  card <- getCard attrs.cardId
  allMatchingDon't \case
    Msg.Discarded target _ _ -> isTarget attrs target
    Do (Msg.Discarded target _ _) -> isTarget attrs target
    _ -> False
  push $ SetCardAside card
  pure $ attrs & tokensL .~ mempty
