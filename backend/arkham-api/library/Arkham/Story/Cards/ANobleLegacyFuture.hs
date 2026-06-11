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
aNobleLegacyFuture =
  storyWith ANobleLegacyFuture Cards.aNobleLegacyFuture (flippedL .~ True) & persistStory

usedAbilities :: StoryAttrs -> [Int]
usedAbilities attrs = toResultDefault [] attrs.meta

instance HasAbilities ANobleLegacyFuture where
  getAbilities (ANobleLegacyFuture a) =
    [ restricted
        a
        1
        ( exists (AssetWithTitle "Thomas Corrigan" <> AssetAt (locationIs Locations.corriganIndustries))
            <> exists (AssetWithTitle "Mary Zielinski" <> AssetAt (locationIs Locations.corriganIndustries))
            <> exists (SetAsideCardMatch $ cardIs Assets.dimensionalBeamMachine)
        )
        actionAbility
    | 1 `notElem` usedAbilities a
    ]
      <> [ restricted
             a
             2
             ( Remembered TeleportationResearchHasBegun
                 <> exists
                   (AssetWithTitle "Thomas Corrigan" <> AssetAt (locationIs Locations.miskatonicUniversityFuture))
                 <> exists
                   (AssetWithTitle "Mary Zielinski" <> AssetAt (locationIs Locations.miskatonicUniversityFuture))
                 <> exists
                   ( assetIs Assets.dimensionalBeamMachine
                       <> AssetAt (locationIs Locations.miskatonicUniversityFuture)
                   )
             )
             actionAbility
         | 2 `notElem` usedAbilities a
         ]
      <> [ restricted
             a
             3
             ( Remembered ThomasAndMaryHaveMadeAHistoricDiscovery
                 <> exists
                   (AssetWithTitle "Thomas Corrigan" <> AssetAt (locationIs Locations.arkhamAdvertiserFuture))
                 <> exists
                   (AssetWithTitle "Mary Zielinski" <> AssetAt (locationIs Locations.arkhamAdvertiserFuture))
             )
             actionAbility
         | 3 `notElem` usedAbilities a
         ]

markUsed :: ReverseQueue m => StoryAttrs -> Int -> m StoryAttrs
markUsed attrs n = do
  let used = nub (n : usedAbilities attrs)
  when (length used == 3) do
    lead <- getLead
    addToVictory lead attrs
  pure $ attrs & metaL .~ toJSON used

instance RunMessage ANobleLegacyFuture where
  runMessage msg s@(ANobleLegacyFuture attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      dbm <- getSetAsideCard Assets.dimensionalBeamMachine
      takeControlOfSetAsideAsset iid dbm
      ANobleLegacyFuture <$> markUsed attrs 1
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) iid #intellect (Fixed 3)
      pure s
    PassedThisSkillTest _iid (isAbilitySource attrs 2 -> True) -> do
      total <- perPlayer 2
      investigatorClues <- selectWithField InvestigatorClues UneliminatedInvestigator
      let totalClues = sum (map snd investigatorClues)
      if totalClues >= total
        then do
          spendCluesAsAGroup (map fst investigatorClues) total
          remember ThomasAndMaryHaveMadeAHistoricDiscovery
          ANobleLegacyFuture <$> markUsed attrs 2
        else pure s
    UseThisAbility _iid (isSource attrs -> True) 3 -> do
      remember ThomasAndMaryHaveWonANobelPrize
      ANobleLegacyFuture <$> markUsed attrs 3
    _ -> ANobleLegacyFuture <$> liftRunMessage msg attrs
