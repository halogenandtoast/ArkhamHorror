module Arkham.Story.Cards.ANobleLegacyPresent (aNobleLegacyPresent) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Query (getLead)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype ANobleLegacyPresent = ANobleLegacyPresent StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aNobleLegacyPresent :: StoryCard ANobleLegacyPresent
aNobleLegacyPresent =
  storyWith ANobleLegacyPresent Cards.aNobleLegacyPresent (flippedL .~ True) & persistStory

usedAbilities :: StoryAttrs -> [Int]
usedAbilities attrs = toResultDefault [] attrs.meta

instance HasAbilities ANobleLegacyPresent where
  getAbilities (ANobleLegacyPresent a) =
    [ restricted a 1 (youAtPresentUniversity <> Remembered FundingForAnObservatoryHasBegun) actionAbility
    | 1 `notElem` usedAbilities a
    ]
      <> [ restricted
             a
             2
             ( Remembered TheObservatoryIsBuilt
                 <> Remembered ThomasAndMaryAreInspiredByNikolaTesla
                 <> scientistsAtPresentUniversity
             )
             $ actionAbilityWithCost (GroupClueCost (PerPlayer 2) Anywhere)
         | 2 `notElem` usedAbilities a
         ]
      <> [ restricted a 3 (Remembered ThomasAndMaryHaveMet <> scientistsAtMagickShoppe)
             $ FastAbility (GroupResourceCost (PerPlayer 1) Anywhere)
         | 3 `notElem` usedAbilities a
         ]
   where
    youAtPresentUniversity =
      youExist $ InvestigatorAt (locationIs Locations.miskatonicUniversityPresent)
    scientistsAtPresentUniversity =
      exists (AssetWithTitle "Thomas Corrigan" <> AssetAt (locationIs Locations.miskatonicUniversityPresent))
        <> exists (AssetWithTitle "Mary Zielinski" <> AssetAt (locationIs Locations.miskatonicUniversityPresent))
        <> exists (AssetWithTitle "Ezra Graves" <> AssetAt (locationIs Locations.miskatonicUniversityPresent))
    scientistsAtMagickShoppe =
      exists (AssetWithTitle "Thomas Corrigan" <> AssetAt (locationIs Locations.yeOldeMagickShoppe))
        <> exists (AssetWithTitle "Mary Zielinski" <> AssetAt (locationIs Locations.yeOldeMagickShoppe))

instance RunMessage ANobleLegacyPresent where
  runMessage msg (ANobleLegacyPresent attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) n | n `elem` [1, 2, 3] -> do
      case n of
        1 -> remember TheObservatoryIsBuilt
        2 -> remember TeleportationResearchHasBegun
        3 -> do
          remember CorriganIndustriesHasBeenFounded
          whenM (selectNone $ locationIs Locations.corriganIndustries) do
            placeSetAsideLocation_ Locations.corriganIndustries
        _ -> pure ()
      let used = nub (n : usedAbilities attrs)
      when (length used == 3) do
        lead <- getLead
        addToVictory lead attrs
      pure $ ANobleLegacyPresent $ attrs & metaL .~ toJSON used
    _ -> ANobleLegacyPresent <$> liftRunMessage msg attrs
