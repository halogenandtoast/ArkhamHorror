module Arkham.Asset.Assets.BrainCase (brainCase) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Enemy.CardDefs.TheBlobThatAteEverything qualified as Enemies
import Arkham.Helpers.Query (getLead)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log (remember)
import Arkham.Placement (Placement (AtLocation, AttachedToEnemy, AttachedToLocation))
import Arkham.ScenarioLogKey (ScenarioLogKey (TheBrainWasTaken))
import Arkham.Story.CardDefs.TheBlobThatAteEverything qualified as Stories

newtype BrainCase = BrainCase AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

brainCase :: AssetCard BrainCase
brainCase = asset BrainCase Cards.brainCase

instance HasAbilities BrainCase where
  getAbilities (BrainCase a) =
    [restricted a 1 criteria actionAbility]
   where
    criteria = case a.placement of
      AttachedToLocation lid -> youExist $ InvestigatorAt $ LocationWithId lid
      AttachedToEnemy eid -> youExist (InvestigatorAt $ locationWithEnemy eid) <> exists (EnemyWithId eid <> ExhaustedEnemy)
      _ -> Never

instance RunMessage BrainCase where
  runMessage msg a@(BrainCase attrs) = runQueueT $ case msg of
    PlaceAsset aid (AttachedToEnemy eid) | aid == attrs.id -> do
      atFungus <-
        eid `matches` (enemyIs Enemies.miGoScientist <> EnemyAt (LocationWithTitle "Fungus Mound"))
      when atFungus do
        remember TheBrainWasTaken
        lead <- getLead
        selectEach (storyIs Stories.reclaimTheBrain) $ push . Flip lead GameSource . StoryTarget
      BrainCase <$> liftRunMessage msg attrs
    PlaceAsset aid (AtLocation lid) | aid == attrs.id && isNothing attrs.controller -> do
      push $ PlaceAsset aid (AttachedToLocation lid)
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseBeginSkillTest sid iid (attrs.ability 1) attrs [#intellect, #agility] (Fixed 6)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      takeControlOfAsset iid attrs
      pure a
    _ -> BrainCase <$> liftRunMessage msg attrs
