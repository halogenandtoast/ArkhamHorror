module Arkham.Homebrew.DarkMatter.Stories.RitualOfTheSun (ritualOfTheSun) where

import Arkham.Ability
import Arkham.Act.Sequence qualified as Act
import Arkham.Agenda.Sequence qualified as Agenda
import Arkham.Calculation
import Arkham.CampaignLogKey (toCampaignLogKey)
import Arkham.Classes.HasGame
import Arkham.GameValue
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Log (getHasRecord)
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.DarkMatter.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.DarkMatter.Key
import Arkham.Matcher
import Arkham.Placement
import Arkham.Story.Import.Lifted

newtype RitualOfTheSun = RitualOfTheSun StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ritualOfTheSun :: StoryCard RitualOfTheSun
ritualOfTheSun = story RitualOfTheSun Cards.ritualOfTheSun

ritualCost :: GameCalculation
ritualCost =
  MultiplyCalculation (GameValueCalculation $ PerPlayer 1)
    $ MinCalculation (Fixed 0)
    $ SubtractCalculation (Fixed 9)
    $ MultiplyCalculation (Fixed 2)
    $ SumCalculation
      [ HasRecordCalculation (toCampaignLogKey YouHaveWitnessedThePrimordialChaos)
      , HasRecordCalculation (toCampaignLogKey YouHaveWitnessedTheUnconsciousPandemonium)
      , HasRecordCalculation (toCampaignLogKey YouHaveWitnessedTheManifestedMadness)
      ]

attachToSol :: HasGame m => StoryAttrs -> m StoryAttrs
attachToSol attrs = do
  msol <- selectOne (locationIs Locations.sol)
  pure $ attrs & maybe id (\lid -> placementL .~ AtLocation lid) msol

instance HasAbilities RitualOfTheSun where
  getAbilities (RitualOfTheSun a) = case a.placement of
    AtLocation _ ->
      [ restricted a 1 (exists $ You <> at_ (connectedTo $ locationIs Locations.sol))
          $ Objective
          $ FastAbility
          $ CalculatedGroupClueCost ritualCost (connectedTo $ locationIs Locations.sol)
      ]
    _ -> [restricted a 2 (exists $ locationIs Locations.sol) $ silent AnyWindow]

instance RunMessage RitualOfTheSun where
  runMessage msg s@(RitualOfTheSun attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 2 -> RitualOfTheSun <$> attachToSol attrs
    ResolveThisStory iid (is attrs -> True) -> do
      knows <- getHasRecord TheInvestigatorsKnowOfTheAbjurationOfTheThrone
      if knows
        then
          RitualOfTheSun . (removeAfterResolutionL .~ False) <$> attachToSol attrs
        else do
          n <- perPlayer 1
          gainClues iid attrs n
          push $ RemoveFromGame (toTarget attrs)
          pure s
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ AdvanceToAgenda 1 Agendas.darkMatter Agenda.C (toSource attrs)
      push $ AdvanceToAct 1 Acts.tassildasAwakening Act.A (toSource attrs)
      selectOne (locationIs Locations.sol) >>= traverse_ (createEnemyAt_ Enemies.tassilda)
      doStep 1 msg
      push $ RemoveFromGame (toTarget attrs)
      pure s
    DoStep 1 (UseThisAbility _ (isSource attrs -> True) 1) -> do
      selectJust AnyAgenda >>= push . SetNoRemainingInvestigatorsHandler . toTarget
      pure s
    _ -> RitualOfTheSun <$> liftRunMessage msg attrs
