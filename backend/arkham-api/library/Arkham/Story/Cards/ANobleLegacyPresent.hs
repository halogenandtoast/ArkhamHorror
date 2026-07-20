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
aNobleLegacyPresent = story ANobleLegacyPresent Cards.aNobleLegacyPresent & persistStory

instance HasAbilities ANobleLegacyPresent where
  getAbilities (ANobleLegacyPresent a) =
    [ restricted
        a
        1
        ( not_ (Remembered TheObservatoryIsBuilt)
            <> youAtPresentUniversity
            <> Remembered FundingForAnObservatoryHasBegun
        )
        actionAbility
    , restricted
        a
        2
        ( not_ (Remembered TeleportationResearchHasBegun)
            <> Remembered TheObservatoryIsBuilt
            <> Remembered ThomasAndMaryAreInspiredByNikolaTesla
            <> scientistsAtPresentUniversity
        )
        $ actionAbilityWithCost (GroupClueCost (PerPlayer 2) Anywhere)
    , restricted
        a
        3
        ( not_ (Remembered CorriganIndustriesHasBeenFounded)
            <> Remembered ThomasAndMaryHaveMet
            <> scientistsAtMagickShoppe
        )
        $ FastAbility (GroupResourceCost (PerPlayer 1) Anywhere)
    , onlyOnce
        $ restricted
          a
          4
          ( Remembered TheObservatoryIsBuilt
              <> Remembered TeleportationResearchHasBegun
              <> Remembered CorriganIndustriesHasBeenFounded
          )
        $ Objective
        $ forced AnyWindow
    ]
   where
    youAtPresentUniversity =
      youExist $ InvestigatorAt (locationIs Locations.miskatonicUniversityPresent)
    scientistsAtPresentUniversity =
      exists
        (AssetWithTitle "Thomas Corrigan" <> AssetAt (locationIs Locations.miskatonicUniversityPresent))
        <> exists
          (AssetWithTitle "Mary Zielinski" <> AssetAt (locationIs Locations.miskatonicUniversityPresent))
        <> exists (AssetWithTitle "Ezra Graves" <> AssetAt (locationIs Locations.miskatonicUniversityPresent))
    scientistsAtMagickShoppe =
      exists (AssetWithTitle "Thomas Corrigan" <> AssetAt (locationIs Locations.yeOldeMagickShoppe))
        <> exists (AssetWithTitle "Mary Zielinski" <> AssetAt (locationIs Locations.yeOldeMagickShoppe))

instance RunMessage ANobleLegacyPresent where
  runMessage msg s@(ANobleLegacyPresent attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      remember TheObservatoryIsBuilt
      pure s
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      remember TeleportationResearchHasBegun
      pure s
    UseThisAbility _iid (isSource attrs -> True) 3 -> do
      remember CorriganIndustriesHasBeenFounded
      whenM (selectNone $ locationIs Locations.corriganIndustries) do
        placeSetAsideLocation_ Locations.corriganIndustries
      pure s
    UseThisAbility _iid (isSource attrs -> True) 4 -> do
      lead <- getLead
      addToVictory lead attrs
      pure s
    FlipThis (isTarget attrs -> True) -> do
      flippedOver attrs
      pure $ ANobleLegacyPresent $ attrs & flippedL .~ True
    _ -> ANobleLegacyPresent <$> liftRunMessage msg attrs
