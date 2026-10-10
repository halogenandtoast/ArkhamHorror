module Arkham.Homebrew.AgesUnwound.Locations.TheTatterdemalion_084 (theTatterdemalion_084) where

import Arkham.Ability
import Arkham.Helpers.Message.Discard.Lifted (randomDiscard)
import Arkham.Helpers.Window (evadedEnemy)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.Unstuck.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype TheTatterdemalion_084 = TheTatterdemalion_084 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | /The Tatterdemalion/, the higher-shroud printing.
theTatterdemalion_084 :: LocationCard TheTatterdemalion_084
theTatterdemalion_084 =
  locationWith TheTatterdemalion_084 Cards.theTatterdemalion_084 4 (PerPlayer 1)
    $ connectsToL
    .~ ringConnections

{- | "[reaction] After you successfully evade an enemy at this location: Defeat
that enemy. (Group limit once per game.)" / "Forced - At the end of your turn:
Test [agility] (3). If you fail, discard a random card from your hand."
-}
instance HasAbilities TheTatterdemalion_084 where
  getAbilities (TheTatterdemalion_084 a) =
    extendRevealed
      a
      [ restricted a endOfTurnAbility Here $ forced $ TurnEnds #when You
      , groupLimit PerGame
          $ restricted a 2 Here
          $ freeReaction (EnemyEvaded #after You $ enemyAt a.id)
      ]

instance RunMessage TheTatterdemalion_084 where
  runMessage msg l@(TheTatterdemalion_084 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #agility (Fixed 3)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      randomDiscard iid (attrs.ability 1)
      pure l
    UseCardAbility iid (isSource attrs -> True) 2 (evadedEnemy -> eid) _ -> do
      push $ DefeatEnemy eid iid (attrs.ability 2)
      pure l
    _ -> TheTatterdemalion_084 <$> liftRunMessage msg attrs
