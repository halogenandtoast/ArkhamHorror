module Arkham.Story.Cards.RecoverTheSample (recoverTheSample) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Placement
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype RecoverTheSample = RecoverTheSample StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

recoverTheSample :: StoryCard RecoverTheSample
recoverTheSample = story RecoverTheSample Cards.recoverTheSample & persistStory

instance HasAbilities RecoverTheSample where
  getAbilities (RecoverTheSample a) =
    [ mkAbility a 1 $ forced $ Moves #after (ControlsAsset $ assetIs Assets.meteoriteSample) AnySource Anywhere (LocationWithTitle "Research Site")
    , restricted
        a
        2
        ( exists
            $ assetIs Assets.meteoriteSample
            <> AssetAttachedTo
              (EnemyTargetMatches $ enemyIs Enemies.miGoHarvester <> EnemyAt (LocationWithTitle "Fungus Mound"))
        )
        $ forced AnyWindow
    ]

instance RunMessage RecoverTheSample where
  runMessage msg s@(RecoverTheSample attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      fungusMound <- getJustLocationByName "Fungus Mound"
      crater <- getJustLocationByName "The Crater"
      createEnemyAt_ Enemies.miGoHarvester fungusMound
      theSample <- getSetAsideCard Assets.meteoriteSample
      createAssetAt_ theSample (AtLocation crater)
      pure $ RecoverTheSample $ attrs & placementL .~ Global
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      remember TheSampleWasRecovered
      flipOver iid attrs
      pure s
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      remember TheSampleWasLost
      flipOver iid attrs
      pure s
    Flip iid _ (isTarget attrs -> True) -> do
      chooseOneM iid $ targeting attrs nothing

      whenM (remembered TheSampleWasRecovered) do
        pet <- getSetAsideCard Assets.petOozeling
        investigators <- allInvestigators
        leadChooseOrRunOneM $ withI18n do
          nameVar Assets.petOozeling $ questionLabeled' "takeControlOf"
          questionLabeledCard Assets.petOozeling
          portraits investigators (`takeControlOfSetAsideAsset` pet)
        selectOne (assetIs Assets.meteoriteSample) >>= traverse_ removeFromGame
        selectOne (enemyIs Enemies.miGoHarvester) >>= traverse_ (addToVictory iid)
        addToVictory iid attrs
        placeTokens (attrs.ability 1) ScenarioTarget #resource 2

      whenM (remembered TheSampleWasLost) do
        selectOne (enemyIs Enemies.miGoHarvester) >>= traverse_ removeFromGame
        selectOne (assetIs Assets.meteoriteSample) >>= traverse_ removeFromGame
        removeFromGame attrs
        removeTokens (attrs.ability 2) ScenarioTarget #resource 2

      pure $ RecoverTheSample $ attrs & flippedL .~ True
    _ -> RecoverTheSample <$> liftRunMessage msg attrs
