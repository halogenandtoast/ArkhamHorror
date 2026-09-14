module Arkham.Story.Cards.TheBlobThatAteEverything.ReclaimTheBrain (reclaimTheBrain) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.CardDefs.TheBlobThatAteEverything qualified as Enemies
import Arkham.Helpers.Query
import Arkham.I18n
import Arkham.Location.CardDefs.TheBlobThatAteEverything qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Placement
import Arkham.ScenarioLogKey
import Arkham.Story.CardDefs.TheBlobThatAteEverything qualified as Cards
import Arkham.Story.Import.Lifted

newtype ReclaimTheBrain = ReclaimTheBrain StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

reclaimTheBrain :: StoryCard ReclaimTheBrain
reclaimTheBrain = story ReclaimTheBrain Cards.reclaimTheBrain & persistStory

instance HasAbilities ReclaimTheBrain where
  getAbilities (ReclaimTheBrain a) =
    [ mkAbility a 1
        $ forced
        $ Moves
          #after
          (ControlsAsset $ assetIs Assets.brainCase)
          AnySource
          Anywhere
          (locationIs Locations.researchSite)
    , restricted
        a
        2
        ( exists
            $ assetIs Assets.brainCase
            <> AssetAttachedTo (EnemyTargetMatches $ enemyIs Enemies.miGoScientist)
        )
        $ forced
        $ EnemyMoves #after (LocationWithTitle "Fungus Mound") (enemyIs Enemies.miGoScientist)
    ]

instance RunMessage ReclaimTheBrain where
  runMessage msg s@(ReclaimTheBrain attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      scientistCard <- getSetAsideCard Enemies.miGoScientist
      locations <- select $ FarthestLocationFromAll LocationCanHaveAttachments
      leadChooseOrRunOneM $ targets locations \loc -> do
        scientist <- createEnemyAt scientistCard loc
        brain <- getSetAsideCard Assets.brainCase
        createAssetAt_ brain (AttachedToEnemy scientist)
      pure $ ReclaimTheBrain $ attrs & placementL .~ Global
    UseThisAbility iid (isSource attrs -> True) 1 -> remember TheBrainWasRecovered >> flipOver iid attrs >> pure s
    UseThisAbility iid (isSource attrs -> True) 2 -> remember TheBrainWasTaken >> flipOver iid attrs >> pure s
    Flip iid _ (isTarget attrs -> True) -> do
      chooseOneM iid $ targeting attrs nothing
      selectEach (assetIs Assets.brainCase) removeFromGame
      recovered <- remembered TheBrainWasRecovered
      if recovered
        then do
          reward <- getSetAsideCard Assets.corrosiveCloud
          investigators <- allInvestigators
          leadChooseOrRunOneM $ portraits investigators (`takeControlOfSetAsideAsset` reward)
          selectEach (enemyIs Enemies.miGoScientist) (addToVictory iid)
          addToVictory iid attrs
          locations <- select $ RevealedLocation <> LocationWithClues AnyValue
          leadChooseOrRunOneM $ portraits investigators \iid' ->
            withI18n
              $ chooseUpToNM iid' 2 "doneDiscoveringClues"
              $ targets locations
              $ discoverAt NotInvestigate iid' (toSource attrs) 1
          push $ PlaceTokens (attrs.ability 1) ScenarioTarget #resource 1
        else do
          selectEach (enemyIs Enemies.miGoScientist) removeFromGame
          removeFromGame attrs
          doStep 3 msg
      pure $ ReclaimTheBrain $ attrs & flippedL .~ True
    DoStep remaining original@(Flip _ _ (isTarget attrs -> True)) | remaining > 0 -> do
      investigators <- select InvestigatorWithAnyClues
      unless (null investigators) $ leadChooseOrRunOneM $ portraits investigators \payer -> do
        spendClues payer 1
        doStep (remaining - 1) original
      pure s
    _ -> ReclaimTheBrain <$> liftRunMessage msg attrs
