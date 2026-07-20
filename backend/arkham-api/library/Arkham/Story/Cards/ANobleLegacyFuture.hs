module Arkham.Story.Cards.ANobleLegacyFuture (aNobleLegacyFuture) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Query (getLead, getSetAsideCard)
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype ANobleLegacyFuture = ANobleLegacyFuture StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aNobleLegacyFuture :: StoryCard ANobleLegacyFuture
aNobleLegacyFuture = story ANobleLegacyFuture Cards.aNobleLegacyFuture & persistStory

instance HasAbilities ANobleLegacyFuture where
  getAbilities (ANobleLegacyFuture a) =
    [ restricted
        a
        1
        ( exists ("Thomas Corrigan" <> AssetAt (locationIs Locations.corriganIndustries))
            <> exists ("Mary Zielinski" <> AssetAt (locationIs Locations.corriganIndustries))
            <> exists (SetAsideCardMatch $ cardIs Assets.dimensionalBeamMachine)
        )
        actionAbility
    , restricted
        a
        2
        ( not_ (Remembered ThomasAndMaryHaveMadeAHistoricDiscovery)
            <> Remembered TeleportationResearchHasBegun
            <> exists ("Thomas Corrigan" <> AssetAt (locationIs Locations.miskatonicUniversityFuture))
            <> exists ("Mary Zielinski" <> AssetAt (locationIs Locations.miskatonicUniversityFuture))
            <> exists
              ( assetIs Assets.dimensionalBeamMachine
                  <> AssetAt (locationIs Locations.miskatonicUniversityFuture)
              )
        )
        actionAbility
    , restricted
        a
        3
        ( not_ (Remembered ThomasAndMaryHaveWonANobelPrize)
            <> Remembered ThomasAndMaryHaveMadeAHistoricDiscovery
            <> exists ("Thomas Corrigan" <> AssetAt (locationIs Locations.arkhamAdvertiserFuture))
            <> exists ("Mary Zielinski" <> AssetAt (locationIs Locations.arkhamAdvertiserFuture))
        )
        actionAbility
    , onlyOnce
        $ restricted a 4 (Remembered ThomasAndMaryHaveWonANobelPrize)
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage ANobleLegacyFuture where
  runMessage msg s@(ANobleLegacyFuture attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      takeControlOfSetAsideAsset iid =<< getSetAsideCard Assets.dimensionalBeamMachine
      pure s
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) iid #intellect (Fixed 3)
      pure s
    PassedThisSkillTest _iid (isAbilitySource attrs 2 -> True) -> do
      total <- perPlayer 2
      investigatorClues <- selectWithField InvestigatorClues UneliminatedInvestigator
      let totalClues = sum (map snd investigatorClues)
      when (totalClues >= total) do
        spendCluesAsAGroup (map fst investigatorClues) total
        remember ThomasAndMaryHaveMadeAHistoricDiscovery
      pure s
    UseThisAbility _iid (isSource attrs -> True) 3 -> do
      remember ThomasAndMaryHaveWonANobelPrize
      pure s
    UseThisAbility _iid (isSource attrs -> True) 4 -> do
      lead <- getLead
      addToVictory lead attrs
      pure s
    FlipThis (isTarget attrs -> True) -> do
      flippedOver attrs
      pure $ ANobleLegacyFuture $ attrs & flippedL .~ True
    _ -> ANobleLegacyFuture <$> liftRunMessage msg attrs
