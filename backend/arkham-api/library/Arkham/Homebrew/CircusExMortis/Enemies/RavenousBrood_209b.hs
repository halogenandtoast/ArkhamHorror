module Arkham.Homebrew.CircusExMortis.Enemies.RavenousBrood_209b (ravenousBrood_209b) where

import Arkham.Card
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.CircusExMortis.Enemies.RavenousBrood_209 (
  broodAbilities,
  setAsideInsteadOfLeavingPlay,
 )
import Arkham.Message (ReplaceStrategy (..))

newtype RavenousBrood_209b = RavenousBrood_209b EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | The alert face. Unlike the hunter face, this one prints its health (2) and spends the
card's X on fight instead: "Ravenous Brood gets +X fight, where X is the number of
players." Everything else is shared with
"Arkham.Homebrew.CircusExMortis.Enemies.RavenousBrood_209".
-}
ravenousBrood_209b :: EnemyCard RavenousBrood_209b
ravenousBrood_209b = enemy RavenousBrood_209b Cards.ravenousBrood_209b

instance HasModifiersFor RavenousBrood_209b where
  getModifiersFor (RavenousBrood_209b a) = do
    n <- getPlayerCount
    modifySelf a [EnemyFight n, CannotHaveAttachments]

instance HasAbilities RavenousBrood_209b where
  getAbilities (RavenousBrood_209b a) = broodAbilities a

instance RunMessage RavenousBrood_209b where
  runMessage msg e@(RavenousBrood_209b attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 ->
      RavenousBrood_209b <$> setAsideInsteadOfLeavingPlay attrs
    -- Each face is its own card, so a flip is a swap to the other one. Nothing in the
    -- engine does this for an enemy: `Flip` has no handler in 'Enemy.Runner'.
    Flip _ _ (isTarget attrs -> True) -> do
      push $ ReplaceEnemy attrs.id (lookupCard Cards.ravenousBrood_209 attrs.cardId) Swap
      pure e
    _ -> RavenousBrood_209b <$> liftRunMessage msg attrs
