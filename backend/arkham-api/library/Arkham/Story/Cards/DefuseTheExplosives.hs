module Arkham.Story.Cards.DefuseTheExplosives (defuseTheExplosives) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Placement
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted
import Arkham.Trait (Trait (Oozified))

newtype DefuseTheExplosives = DefuseTheExplosives StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

defuseTheExplosives :: StoryCard DefuseTheExplosives
defuseTheExplosives = story DefuseTheExplosives Cards.defuseTheExplosives & persistStory

instance HasAbilities DefuseTheExplosives where
  getAbilities (DefuseTheExplosives a) =
    guard (toResultDefault False a.meta)
      *> [ onlyOnce
             $ restricted a 1 (not_ $ exists $ LocationWithAnyHorror <> LocationWithTrait Oozified)
             $ forced AnyWindow
         , onlyOnce
             $ restricted a 2 (not_ $ exists $ assetIs Assets.theMilitarysPlan <> AssetWithDamage)
             $ forced AnyWindow
         ]

instance RunMessage DefuseTheExplosives where
  runMessage msg s@(DefuseTheExplosives attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      fungusMound <- getJustLocationByName "Fungus Mound"
      createEnemyAt_ Enemies.miGoMeddler fungusMound
      plan <- getSetAsideCard Assets.theMilitarysPlan
      createAssetAt_ plan Global
      n <- getPlayerCount
      doStep (1 + n) msg
      pure $ DefuseTheExplosives $ attrs & placementL .~ Global
    DoStep n msg'@(ResolveThisStory _ (is attrs -> True)) | n > 1 -> do
      crater <- getJustLocationByName "The Crater"
      locations <-
        select
          $ FarthestLocationFromLocation
            crater
            (LocationWithTrait Oozified <> LocationCanHaveAttachments <> not_ (LocationWithToken #horror))
      if length locations <= n
        then do
          for_ locations \lid -> placeTokens attrs lid #horror 1
          doStep (n - length locations) msg'
        else do
          leadChooseOneM $ targets locations \lid -> placeTokens attrs lid #horror 1
          doStep (n - 1) msg'
      pure s
    DoStep 1 msg'@(ResolveThisStory _ (is attrs -> True)) -> do
      selectOne (assetIs Assets.theMilitarysPlan) >>= traverse_ \aid -> placeTokens attrs aid #damage 5
      doStep 0 msg'
      pure s
    DoStep 0 (ResolveThisStory _ (is attrs -> True)) -> do
      pure $ DefuseTheExplosives $ attrs & metaL .~ toJSON True
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      remember TheExplosivesWereDefused
      flipOver iid attrs
      pure s
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      remember TheExplosivesWereDetonated
      flipOver iid attrs
      pure s
    Flip iid _ (isTarget attrs -> True) -> do
      chooseOneM iid $ targeting attrs nothing
      whenM (remembered TheExplosivesWereDefused) do
        stewart <- getSetAsideCard Assets.ltWilsonStewart
        investigators <- getInvestigators
        leadChooseOrRunOneM $ targets investigators (`takeControlOfSetAsideAsset` stewart)
        selectOne (assetIs Assets.theMilitarysPlan) >>= traverse_ removeFromGame
        selectOne (enemyIs Enemies.miGoMeddler) >>= traverse_ (addToVictory iid)
        addToVictory iid attrs
        eachInvestigator \iid' -> healHorror iid' (attrs.ability 1) 1
        placeTokens (attrs.ability 1) ScenarioTarget #resource 1

      whenM (remembered TheExplosivesWereDetonated) do
        selectEach (enemyIs Enemies.miGoMeddler) removeFromGame
        selectEach (assetIs Assets.theMilitarysPlan) removeFromGame
        selectEach (LocationWithAnyHorror <> LocationWithTrait Oozified) \lid -> do
          removeTokens (attrs.ability 2) lid #horror 1
          selectEach (InvestigatorAt $ LocationWithId lid) \iid' -> directDamage iid' attrs 3
          selectEach (AssetAt (LocationWithId lid) <> #ally) \aid -> dealAssetDamage aid (attrs.ability 2) 3
        selectEach (enemyIs Enemies.subject8L08) \eid -> healDamage eid (attrs.ability 2) 5
        removeFromGame attrs
      pure $ DefuseTheExplosives $ attrs & flippedL .~ True & metaL .~ toJSON False
    _ -> DefuseTheExplosives <$> liftRunMessage msg attrs
