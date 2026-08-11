module Arkham.Homebrew.DarkMatter.Enemies.VirtualByakhee (virtualByakhee) where

import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype VirtualByakhee = VirtualByakhee EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | "Spawn - Furthest location from you. Hunter.

TODO(homebrew): "Forced - When Virtual Byakhee would move between two locations
without investigators: Switch those locations with each other instead." The
engine has no would-move-between window carrying both endpoints for a hunter
move, so the substitution is not modeled; the byakhee simply moves.
-}
virtualByakhee :: EnemyCard VirtualByakhee
virtualByakhee =
  enemy VirtualByakhee Cards.virtualByakhee
    & setSpawnAt (FarthestLocationFromYou Anywhere)

instance HasModifiersFor VirtualByakhee where
  getModifiersFor (VirtualByakhee a) = modifySelf a [AddKeyword Keyword.Hunter]

instance RunMessage VirtualByakhee where
  runMessage msg (VirtualByakhee attrs) =
    runQueueT $ VirtualByakhee <$> liftRunMessage msg attrs
