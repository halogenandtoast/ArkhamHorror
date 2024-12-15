module Arkham.Enemy.Cards.Nightriders (nightriders, Nightriders (..)) where

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Modifier qualified as Mod
import Arkham.Prelude

newtype Nightriders = Nightriders EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nightriders :: EnemyCard Nightriders
nightriders = enemy Nightriders Cards.nightriders (2, Static 2, 5) (0, 1)

instance HasModifiersFor Nightriders where
  getModifiersFor (Nightriders a) = do
    isHost <- toId a <=~> IsHost
    noSwarm <- selectNone $ SwarmOf (toId a)
    modifySelfWhen a (isHost && noSwarm) [Mod.EnemyEvade (-5)]

instance HasAbilities Nightriders where
  getAbilities (Nightriders a) =
    extend a [mkAbility a 1 $ forced $ EnemyEvaded #after You (be a)]

instance RunMessage Nightriders where
  runMessage msg e@(Nightriders attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ toDiscardBy iid (attrs.ability 1) attrs
      pure e
    _ -> Nightriders <$> runMessage msg attrs
