module Arkham.Homebrew.AgesUnwound.Enemies.Smilodon (smilodon) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.ForMovement (ForMovement (NotForMovement))
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Cards
import Arkham.Matcher

newtype Smilodon = Smilodon EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | /Smilodon/ (@:ages-unwound:125@). "__Spawn__ - Any connecting location
(empty, if able)."

The "(empty, if able)" preference is a 'SpawnAtFirst' of two matchers, the same
shape The Inescapable uses for the identical sentence: an empty connecting
location first, any connecting location otherwise.
-}
smilodon :: EnemyCard Smilodon
smilodon =
  enemyWith Smilodon Cards.smilodon
    $ spawnAtL
    ?~ SpawnAtFirst
      [ SpawnAt $ ConnectedLocation NotForMovement <> EmptyLocation
      , SpawnAt $ ConnectedLocation NotForMovement
      ]

{- | "__Forced__ - After Smilodon moves via its hunter keyword: It gets +1 damage
and +1 horror until the end of the round."
-}
instance HasAbilities Smilodon where
  getAbilities (Smilodon a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyMovedTo #after Anywhere #hunter (be a)

instance RunMessage Smilodon where
  runMessage msg e@(Smilodon attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      roundModifiers (attrs.ability 1) attrs [DamageDealt 1, HorrorDealt 1]
      pure e
    _ -> Smilodon <$> liftRunMessage msg attrs
