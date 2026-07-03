module Arkham.Enemy.Cards.VileBroodmaster (vileBroodmaster) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher
import Arkham.Scenarios.WarOfTheOuterGods.Helpers
import Arkham.Token
import Arkham.Window qualified as Window

newtype VileBroodmaster = VileBroodmaster EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vileBroodmaster :: EnemyCard VileBroodmaster
vileBroodmaster = enemy VileBroodmaster Cards.vileBroodmaster

instance HasModifiersFor VileBroodmaster where
  getModifiersFor (VileBroodmaster a) = do
    mutations <- getMutations a.id
    modifySelf a [HealthModifier mutations | mutations > 0]

instance HasAbilities VileBroodmaster where
  getAbilities (VileBroodmaster a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ PlacedToken #when (NotSource $ SourceIs $ a.ability 1) (EnemyTargetMatches AnyEnemy) Mutation

instance RunMessage VileBroodmaster where
  runMessage msg e@(VileBroodmaster attrs) = runQueueT $ case msg of
    -- When 1 or more mutations would be placed on an enemy, place 1
    -- additional mutation on that enemy.
    UseCardAbility _ (isSource attrs -> True) 1 windows _ -> do
      let
        enemies =
          [eid | (Window.windowType -> Window.PlacedToken _ (EnemyTarget eid) Mutation _) <- windows]
      for_ enemies \eid -> placeMutations (attrs.ability 1) eid 1
      pure e
    _ -> VileBroodmaster <$> liftRunMessage msg attrs
