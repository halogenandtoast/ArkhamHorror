module Arkham.Enemy.Cards.TheDrownedCity.TheDrownedQuarter.UnderseaParasite (underseaParasite) where

import Arkham.Ability
import Arkham.Enemy.CardDefs.TheDrownedCity.TheDrownedQuarter qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks, pattern EnemyAttacks)
import Arkham.Helpers.Story
import Arkham.Matcher
import Arkham.Scenarios.TheDrownedCity.TheDrownedQuarter.Helpers (UnderseaParasiteFlip (..))
import Arkham.Story.CardDefs.TheDrownedCity.TheDrownedQuarter qualified as Stories

newtype UnderseaParasite = UnderseaParasite EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

underseaParasite :: EnemyCard UnderseaParasite
underseaParasite = enemy UnderseaParasite Cards.underseaParasite

instance HasAbilities UnderseaParasite where
  getAbilities (UnderseaParasite a) =
    extend
      a
      [ mkAbility a 1 $ forced $ EnemyAttacks #after You AnyEnemyAttack (be a)
      , mkAbility a 2 $ forced $ EnemyLeavesPlay #when (be a)
      ]

instance RunMessage UnderseaParasite where
  runMessage msg (UnderseaParasite attrs) = runQueueT $ case msg of
    -- Both abilities flip to the same story back (11549b), whose text branches on
    -- which one flipped it. Record the reason in meta first; the story reads it
    -- back off this enemy and resolves the matching half.
    UseThisAbility iid (isSource attrs -> True) n | n `elem` [1, 2] -> do
      readStoryWithPlacement iid attrs Stories.underseaParasite (enemyPlacement attrs)
      let flippedBy = if n == 1 then FlippedByAttack else FlippedByLeavingPlay
      pure $ UnderseaParasite $ setMeta flippedBy attrs
    _ -> UnderseaParasite <$> liftRunMessage msg attrs
