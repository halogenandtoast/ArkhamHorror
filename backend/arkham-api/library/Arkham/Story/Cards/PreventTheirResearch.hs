module Arkham.Story.Cards.PreventTheirResearch (preventTheirResearch) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (EnemyTokens))
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Placement
import Arkham.Projection (fieldMap)
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted
import Arkham.Token qualified as Token
import Arkham.Trait (Trait (Ooze))

newtype PreventTheirResearch = PreventTheirResearch StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

preventTheirResearch :: StoryCard PreventTheirResearch
preventTheirResearch = story PreventTheirResearch Cards.preventTheirResearch & persistStory

instance HasAbilities PreventTheirResearch where
  getAbilities (PreventTheirResearch a) =
    [ restricted a 1 (not_ $ exists $ enemyIs Enemies.miGoResearcher)
        $ forced
        $ EnemyLeavesPlay #after (enemyIs Enemies.miGoResearcher)
    ]

instance RunMessage PreventTheirResearch where
  runMessage msg s@(PreventTheirResearch attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      fungus <- getJustLocationByName "Fungus Mound"
      createEnemyAt_ Enemies.miGoResearcher fungus
      locations <- select $ FarthestLocationFromAll LocationCanHaveAttachments
      leadChooseOrRunOneM $ targets locations $ createEnemyAt_ Enemies.miGoResearcher
      pure $ PreventTheirResearch $ attrs & placementL .~ Global
    PlaceTokens _ (isTarget attrs -> True) Token.Resource n
      | Token.countTokens Token.Resource attrs.tokens + n >= 6 -> do
          attrs' <- liftRunMessage msg attrs
          lead <- getLead
          remember TheMiGoCompletedTheirResearch
          flipOver lead attrs
          pure $ PreventTheirResearch attrs'
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      remember TheMiGoResearchWasStopped
      flipOver iid attrs
      pure s
    Flip iid _ (isTarget attrs -> True) -> do
      chooseOneM iid $ targeting attrs nothing
      stopped <- remembered TheMiGoResearchWasStopped
      if stopped
        then do
          reward <- getSetAsideCard Assets.alienInstruments
          investigators <- allInvestigators
          leadChooseOrRunOneM $ portraits investigators (`takeControlOfSetAsideAsset` reward)
          selectEach (enemyIs Enemies.miGoResearcher) (addToVictory iid)
          addToVictory iid attrs
          locations <- select $ RevealedLocation <> LocationWithClues AnyValue
          leadChooseOrRunOneM $ portraits investigators \iid' ->
            chooseOrRunOneM iid' (targets locations $ discoverAt NotInvestigate iid' (toSource attrs) 1)
        else do
          selectEach (EnemyWithTrait Ooze) \enemy -> do
            researched <- fieldMap EnemyTokens (Token.countTokens Token.Resource) enemy
            when (researched > 0) do
              readyThis enemy
              push $ HealAllDamage (EnemyTarget enemy) (toSource attrs)
          selectEach (enemyIs Enemies.miGoResearcher) removeFromGame
          removeFromGame attrs
          investigators <- select InvestigatorWithAnyClues
          leadChooseOrRunOneM $ portraits investigators (`spendClues` 1)
      pure $ PreventTheirResearch $ attrs & flippedL .~ True
    _ -> PreventTheirResearch <$> liftRunMessage msg attrs
