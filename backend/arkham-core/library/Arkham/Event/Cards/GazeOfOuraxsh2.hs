module Arkham.Event.Cards.GazeOfOuraxsh2 (gazeOfOuraxsh2, GazeOfOuraxsh2 (..)) where

import Arkham.ChaosBag.RevealStrategy
import Arkham.Classes
import Arkham.DamageEffect
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.RequestedChaosTokenStrategy

newtype GazeOfOuraxsh2 = GazeOfOuraxsh2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gazeOfOuraxsh2 :: EventCard GazeOfOuraxsh2
gazeOfOuraxsh2 = event GazeOfOuraxsh2 Cards.gazeOfOuraxsh2

instance RunMessage GazeOfOuraxsh2 where
  runMessage msg e@(GazeOfOuraxsh2 attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      push $ RequestChaosTokens (toSource attrs) (Just iid) (Reveal 7) SetAside
      pure e
    RequestedChaosTokens (isSource attrs -> True) (Just iid) tokens -> do
      let n = 1 + count ((`elem` [#curse, #autofail]) . (.face)) tokens
      enemies <-
        select $ enemyAtLocationWith iid <> EnemyCanBeDamagedBySource (toSource attrs)
      player <- getPlayer iid

      when (notNull enemies) do
        pushAll
          $ replicate
            n
            ( chooseOne
                player
                [targetLabel enemy [EnemyDamage enemy $ delayDamage $ nonAttack attrs 1] | enemy <- enemies]
            )
          <> map (checkDefeated attrs) enemies
          <> [ResetChaosTokens (toSource attrs)]
      pure e
    _ -> GazeOfOuraxsh2 <$> runMessage msg attrs
