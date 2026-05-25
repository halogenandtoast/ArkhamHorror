module Arkham.Enemy.Cards.CavernMoss (cavernMoss) where

import Arkham.Ability
import Arkham.Asset.Types (Field (..))
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Projection
import Arkham.Trait (Trait (Item))

newtype CavernMoss = CavernMoss EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cavernMoss :: EnemyCard CavernMoss
cavernMoss = enemy CavernMoss Cards.cavernMoss (2, Static 3, 1) (1, 0)

instance HasModifiersFor CavernMoss where
  getModifiersFor (CavernMoss a) = case a.placement of
    AttachedToAsset aid _ -> do
      modified_ a aid [Blank]
      field AssetController aid >>= traverse_ \iid ->
        modified_ a iid [AsIfEngagedWith a.id]
    _ -> pure ()

instance HasAbilities CavernMoss where
  getAbilities (CavernMoss a) = case a.placement of
    AttachedToAsset {} -> getAbilities a
    _ ->
      extend1 a
        $ restricted a 1 (EnemyCriteria $ ThisEnemy ReadyEnemy)
        $ forced
        $ SkillTestResult
          #after
          You
          ( SkillTestOneOf [SkillTestWithAction #fight, SkillTestWithAction #investigate]
              <> SkillTestAt (locationWithEnemy a)
          )
          #success

instance RunMessage CavernMoss where
  runMessage msg e@(CavernMoss attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assets <- select $ assetControlledBy iid <> AssetWithTrait Item
      chooseTargetM iid assets \aid ->
        push $ PlaceEnemy attrs.id (AttachedToAsset aid Nothing)
      pure e
    _ -> CavernMoss <$> liftRunMessage msg attrs
