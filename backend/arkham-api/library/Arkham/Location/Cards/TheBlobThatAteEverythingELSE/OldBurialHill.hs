module Arkham.Location.Cards.TheBlobThatAteEverythingELSE.OldBurialHill (oldBurialHill) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (ScenarioModifier))
import Arkham.Helpers.Window (spawnedEnemy)
import Arkham.Location.CardDefs.TheBlobThatAteEverythingELSE qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (Manifold))

newtype OldBurialHill = OldBurialHill LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oldBurialHill :: LocationCard OldBurialHill
oldBurialHill = locationWith OldBurialHill Cards.oldBurialHill 4 (PerPlayer 1) connectsToAdjacent

instance HasAbilities OldBurialHill where
  getAbilities (OldBurialHill a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ EnemySpawns #after (be a) (EnemyWithTrait Manifold)

instance RunMessage OldBurialHill where
  runMessage msg l@(OldBurialHill attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (spawnedEnemy -> enemy) _ -> do
      exhaustThis enemy
      roundModifier (attrs.ability 1) enemy (ScenarioModifier "oldBurialHillBlob")
      pure l
    _ -> OldBurialHill <$> liftRunMessage msg attrs
