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

usedAbilities :: StoryAttrs -> [Int]
usedAbilities attrs = toResultDefault [] attrs.meta

instance HasAbilities ANobleLegacyPast where
  getAbilities (ANobleLegacyPast a) =
    [ restricted a 1 (youAtPastUniversity <> thomasAtPastUniversity <> maryAtPastUniversity) actionAbility
    | 1 `notElem` usedAbilities a
    ]
      <> [ restricted a 2 scientistsTogether
             $ actionAbilityWithCost (GroupClueCost (PerPlayer 2) Anywhere)
         | 2 `notElem` usedAbilities a
         ]
      <> [ restricted a 3 youAtPastUniversity
             $ FastAbility (GroupResourceCost (PerPlayer 2) (locationIs Locations.miskatonicUniversityPast))
         | 3 `notElem` usedAbilities a
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
  runMessage msg (ANobleLegacyPast attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) n | n `elem` [1, 2, 3] -> do
      case n of
        1 -> remember ThomasAndMaryHaveMet
        2 -> remember ThomasAndMaryAreInspiredByNikolaTesla
        3 -> remember FundingForAnObservatoryHasBegun
        _ -> pure ()
      let used = nub (n : usedAbilities attrs)
      when (length used == 3) do
        lead <- getLead
        addToVictory lead attrs
      pure $ ANobleLegacyPast $ attrs & metaL .~ toJSON used
    _ -> ANobleLegacyPast <$> liftRunMessage msg attrs
