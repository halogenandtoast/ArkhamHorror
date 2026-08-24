module Arkham.Homebrew.DarkMatter.Stories.RitualOfTheSun (ritualOfTheSun) where

import Arkham.Ability
import Arkham.Calculation
import Arkham.CampaignLogKey (toCampaignLogKey)
import Arkham.Card (toCard)
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

{- | "Objective - Investigators at a connecting location must spend
9[per_investigator] clues, as a group, to complete the ritual. If you do so,
replace the agenda deck and act deck with the set aside Dark Matter agenda and
Tassilda's Awakening act, and spawn the set aside Tassilda enemy at Sol."
-}

{- | "Investigators at a connecting location must spend 9[per_investigator] clues,
as a group... Reduce the cost of the ritual by 2[per_investigator] for each of
the following entries in your Campaign Log: you witnessed the primordial chaos /
the unconscious pandemonium / the manifested madness."
-}
ritualCost :: GameCalculation
ritualCost =
  MultiplyCalculation (CountInvestigators UneliminatedInvestigator)
    $ MaxCalculation (Fixed 0)
    $ SubtractCalculation (Fixed 9)
    $ MultiplyCalculation (Fixed 2)
    $ SumCalculation
      [ HasRecordCalculation (toCampaignLogKey YouHaveWitnessedThePrimordialChaos)
      , HasRecordCalculation (toCampaignLogKey YouHaveWitnessedTheUnconsciousPandemonium)
      , HasRecordCalculation (toCampaignLogKey YouHaveWitnessedTheManifestedMadness)
      ]

instance HasAbilities RitualOfTheSun where
  getAbilities (RitualOfTheSun a) =
    [ restricted a 1 (exists $ You <> at_ (connectedTo $ locationIs Locations.sol))
        $ Objective
        $ FastAbility
        $ CalculatedGroupClueCost ritualCost (connectedTo $ locationIs Locations.sol)
    ]

instance RunMessage RitualOfTheSun where
  runMessage msg s@(RitualOfTheSun attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      knows <- getHasRecord TheInvestigatorsKnowOfTheAbjurationOfTheThrone
      if knows
        then
          selectOne (locationIs Locations.sol) >>= traverse_ \lid ->
            push $ PlaceStory (toCard attrs) (AttachedToLocation lid)
        else do
          -- "Gain 1[per_investigator] clues from the token bank. Remove this card from the game."
          n <- perPlayer 1
          gainClues iid attrs n
          push $ RemoveFromGame (toTarget attrs)
      pure s
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      setAgendaDeck [Agendas.darkMatter]
      setActDeck [Acts.tassildasAwakening]
      selectOne (locationIs Locations.sol) >>= traverse_ (createEnemyAt_ Enemies.tassilda)
      push $ RemoveFromGame (toTarget attrs)
      pure s
    _ -> RitualOfTheSun <$> liftRunMessage msg attrs
