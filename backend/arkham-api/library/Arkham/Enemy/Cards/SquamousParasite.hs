module Arkham.Enemy.Cards.SquamousParasite (squamousParasite) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Story (readStoryWithPlacement)
import Arkham.Matcher
import Arkham.Message.Lifted.Placement
import Arkham.Story.Cards qualified as Stories

newtype SquamousParasite = SquamousParasite EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

squamousParasite :: EnemyCard SquamousParasite
squamousParasite = enemy SquamousParasite Cards.squamousParasite

instance HasAbilities SquamousParasite where
  getAbilities (SquamousParasite a) =
    extend
      a
      [ mkAbility a 1 $ forced $ EnemyDefeated #when You ByAny (be a)
      , mkAbility a 2 $ SilentForcedAbility $ EnemyLeavesPlay #when (be a)
      ]

instance RunMessage SquamousParasite where
  runMessage msg e@(SquamousParasite attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- "Flip this enemy and resolve its text." The glyph back (11580b) reads as a
      -- story placed where the enemy is, so the UI shows it in the enemy's slot.
      -- The story adds itself to the victory display and quietly removes this
      -- enemy, so only one card is ever left over.
      readStoryWithPlacement iid attrs Stories.squamousParasite (enemyPlacement attrs)
      pure e
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      -- "If Squamous Parasite would leave play, set it aside, out of play."
      place attrs (OutOfPlay SetAsideZone)
      pure e
    _ -> SquamousParasite <$> liftRunMessage msg attrs
