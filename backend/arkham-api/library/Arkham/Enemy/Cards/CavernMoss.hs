module Arkham.Enemy.Cards.CavernMoss (cavernMoss) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modified_, modifySelect, modifySelf)
import Arkham.Keyword (Keyword (Aloof))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement

newtype CavernMoss = CavernMoss EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cavernMoss :: EnemyCard CavernMoss
cavernMoss = enemy CavernMoss Cards.cavernMoss

instance HasModifiersFor CavernMoss where
  getModifiersFor (CavernMoss a) = case a.placement of
    AttachedToAsset aid _ -> do
      modifySelf
        a
        [ CannotBeMoved
        , CannotMove
        , CannotBeEvaded
        , CannotAttack
        , CannotMakeAttacksOfOpportunity
        , CannotBeEngaged
        , RemoveKeyword Aloof
        ]
      modified_ a aid [Blank]
      modifySelect a (InvestigatorAt (locationWithEnemy a)) [AsIfEngagedWith a.id]
    _ -> pure ()

instance HasAbilities CavernMoss where
  getAbilities (CavernMoss a) =
    extend1 a
      $ restricted a 1 (thisExists a ReadyEnemy <> exists (assetWrapper $ AssetControlledBy You <> #item))
      $ forced
      $ SkillTestResult
        #after
        You
        ( SkillTestOneOf [SkillTestWithAction #fight, SkillTestWithAction #investigate]
            <> SkillTestAt (locationWithEnemy a)
        )
        #success
   where
    assetWrapper = case a.placement of
      AttachedToAsset aid _ -> (<> not_ (AssetWithId aid))
      _ -> id

instance RunMessage CavernMoss where
  runMessage msg e@(CavernMoss attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let
        assetWrapper = case attrs.placement of
          AttachedToAsset aid _ -> (<> not_ (AssetWithId aid))
          _ -> id
      assets <- select $ assetWrapper $ assetControlledBy iid <> #item
      chooseTargetM iid assets \aid ->
        push $ PlaceEnemy attrs.id (AttachedToAsset aid Nothing)
      pure e
    _ -> CavernMoss <$> liftRunMessage msg attrs
