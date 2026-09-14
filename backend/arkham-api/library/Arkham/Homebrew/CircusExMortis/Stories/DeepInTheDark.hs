module Arkham.Homebrew.CircusExMortis.Stories.DeepInTheDark (deepInTheDark) where

import Arkham.Ability
import Arkham.GameValue (GameValue (..))
import Arkham.Homebrew.CircusExMortis.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (moonToken)
import Arkham.Id (LocationId)
import Arkham.Matcher
import Arkham.Placement
import Arkham.Story.Import.Lifted

newtype DeepInTheDark = DeepInTheDark StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | Starts Kidnapped Citizen (b) side up; flips to the story side and back.
deepInTheDark :: StoryCard DeepInTheDark
deepInTheDark = storyWith DeepInTheDark Cards.deepInTheDark (flippedL .~ True)

{- | "Reduce the amount of clues required for this action by 1 for each ☾ token
sealed on a player card at this location." The player cards at a location are the
investigators there and the assets there, the two pools 'getSealedMoonTokensAt'
reads. Counted as two calculations rather than one 'ChaosTokenMatchesAny',
because only a top-level @SealedOn*@ puts sealed tokens in the candidate pool.
-}
clueCostAt :: LocationId -> GameCalculation
clueCostAt lid =
  MinCalculation (Fixed 0)
    $ SubtractCalculation (GameValueCalculation (PerPlayer 1))
    $ SumCalculation
      [ CountChaosTokens $ SealedOnInvestigator (InvestigatorAt $ LocationWithId lid) moonToken
      , CountChaosTokens $ SealedOnAsset (AssetAtLocation lid) moonToken
      ]

instance HasAbilities DeepInTheDark where
  getAbilities (DeepInTheDark attrs)
    | attrs.flipped =
        [restricted attrs 1 OnSameLocation $ freeTrigger (GroupClueCost (PerPlayer 1) YourLocation)]
    | otherwise = case attrs.placement of
        AtLocation lid ->
          [ restricted attrs 2 OnSameLocation
              $ actionAbilityWithCost
              $ CalculatedGroupClueCost (clueCostAt lid) (LocationWithId lid)
          ]
        _ -> []

instance RunMessage DeepInTheDark where
  runMessage msg (DeepInTheDark attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      pure . DeepInTheDark $ attrs & flippedL .~ False
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      addToVictory iid attrs
      pure . DeepInTheDark $ attrs & flippedL .~ True
    _ -> DeepInTheDark <$> liftRunMessage msg attrs
