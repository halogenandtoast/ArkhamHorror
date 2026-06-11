module Arkham.Enemy.Cards.EdwinBennetBitterAdversary (edwinBennetBitterAdversary) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelf)
import Arkham.Helpers.Query (getLead)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Scientist))

newtype EdwinBennetBitterAdversary = EdwinBennetBitterAdversary EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

edwinBennetBitterAdversary :: EnemyCard EdwinBennetBitterAdversary
edwinBennetBitterAdversary =
  enemy EdwinBennetBitterAdversary Cards.edwinBennetBitterAdversary (2, Static 1, 2) (1, 1)

instance HasModifiersFor EdwinBennetBitterAdversary where
  getModifiersFor (EdwinBennetBitterAdversary a) = do
    self <- modifySelf a [CannotBeDamaged]
    others <-
      modifySelect
        a
        (InvestigatorAt $ locationWithEnemy a.id)
        [CannotDiscoverCluesAt $ locationWithEnemy a.id]
    pure $ self <> others

instance HasAbilities EdwinBennetBitterAdversary where
  getAbilities (EdwinBennetBitterAdversary a) =
    extend1 a
      $ restricted a 1 (exists $ AssetWithTrait Scientist <> AssetAt (locationWithEnemy a.id))
      $ forced
      $ PhaseEnds #when #mythos

instance RunMessage EdwinBennetBitterAdversary where
  runMessage msg e@(EdwinBennetBitterAdversary attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      withLocationOf attrs \lid -> do
        scientists <- select $ AssetWithTrait Scientist <> AssetAt (LocationWithId lid)
        lead <- getLead
        chooseTargetM lead scientists \scientist ->
          dealAssetDamage scientist (attrs.ability 1) 1
      pure e
    _ -> EdwinBennetBitterAdversary <$> liftRunMessage msg attrs
