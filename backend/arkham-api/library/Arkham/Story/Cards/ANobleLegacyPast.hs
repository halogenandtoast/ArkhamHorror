module Arkham.Story.Cards.ANobleLegacyPast (aNobleLegacyPast) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Query (getLead)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype ANobleLegacyPast = ANobleLegacyPast StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aNobleLegacyPast :: StoryCard ANobleLegacyPast
aNobleLegacyPast = storyWith ANobleLegacyPast Cards.aNobleLegacyPast (flippedL .~ True) & persistStory

instance HasAbilities ANobleLegacyPast where
  getAbilities (ANobleLegacyPast a) =
    [ restricted
        a
        1
        ( not_ (Remembered ThomasAndMaryHaveMet)
            <> youAtPastUniversity
            <> thomasAtPastUniversity
            <> maryAtPastUniversity
        )
        actionAbility
    , restricted a 2 (not_ (Remembered ThomasAndMaryAreInspiredByNikolaTesla) <> scientistsTogether)
        $ actionAbilityWithCost (GroupClueCost (PerPlayer 2) Anywhere)
    , restricted a 3 (youAtPastUniversity <> not_ (Remembered FundingForAnObservatoryHasBegun))
        $ FastAbility (GroupResourceCost (PerPlayer 2) Anywhere)
    , onlyOnce
        $ restricted
          a
          4
          ( Remembered ThomasAndMaryHaveMet
              <> Remembered ThomasAndMaryAreInspiredByNikolaTesla
              <> Remembered FundingForAnObservatoryHasBegun
          )
        $ Objective
        $ forced AnyWindow
    ]
   where
    youAtPastUniversity = youExist $ InvestigatorAt (locationIs Locations.miskatonicUniversityPast)
    thomasAtPastUniversity =
      exists $ AssetWithTitle "Thomas Corrigan" <> AssetAt (locationIs Locations.miskatonicUniversityPast)
    maryAtPastUniversity =
      exists $ AssetWithTitle "Mary Zielinski" <> AssetAt (locationIs Locations.miskatonicUniversityPast)
    scientistsTogether =
      exists
        $ AssetWithTitle "Thomas Corrigan"
        <> AssetAt
          ( LocationWithAsset (AssetWithTitle "Mary Zielinski")
              <> LocationWithAsset (AssetWithTitle "Nikola Tesla")
          )

instance RunMessage ANobleLegacyPast where
  runMessage msg s@(ANobleLegacyPast attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      remember ThomasAndMaryHaveMet
      pure s
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      remember ThomasAndMaryAreInspiredByNikolaTesla
      pure s
    UseThisAbility _iid (isSource attrs -> True) 3 -> do
      remember FundingForAnObservatoryHasBegun
      pure s
    UseThisAbility _iid (isSource attrs -> True) 4 -> do
      lead <- getLead
      addToVictory lead attrs
      pure s
    _ -> ANobleLegacyPast <$> liftRunMessage msg attrs
