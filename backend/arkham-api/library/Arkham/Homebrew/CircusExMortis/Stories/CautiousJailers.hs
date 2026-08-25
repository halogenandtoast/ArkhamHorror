module Arkham.Homebrew.CircusExMortis.Stories.CautiousJailers (cautiousJailers) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Homebrew.CircusExMortis.CardDefs.Stories qualified as Cards
import Arkham.Matcher
import Arkham.Placement
import Arkham.Story.Import.Lifted

newtype CautiousJailers = CautiousJailers StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Enters play showing its Kidnapped Citizen face; the story face is revealed
by ability 1 and flips back when the captive is freed.
-}
cautiousJailers :: StoryCard CautiousJailers
cautiousJailers = storyWith CautiousJailers Cards.cautiousJailers (flippedL .~ True) & persistStory

instance HasAbilities CautiousJailers where
  getAbilities (CautiousJailers a)
    | a.flipped =
        [restricted a 1 OnSameLocation $ freeTrigger (GroupClueCost (PerPlayer 1) YourLocation)]
    | otherwise = case a.placement of
        AtLocation lid ->
          [ -- The attacked investigator is the one offered this, so a single attack
            -- places a single resource no matter how many investigators are here.
            mkAbility a 2
              $ freeReaction
              $ EnemyAttacksEvenIfCancelled
                #after
                (You <> InvestigatorAt (LocationWithId lid))
                AnyEnemyAttack
                (EnemyWithTitle "Towering Dark Young")
          , restricted a 3 OnSameLocation $ freeTrigger (clueCost 1)
          ]
        _ -> []

instance RunMessage CautiousJailers where
  runMessage msg s@(CautiousJailers attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 ->
      pure . CautiousJailers $ attrs & flippedL .~ False
    UseThisAbility _ (isSource attrs -> True) n | n `elem` [2, 3] -> do
      placeTokens (attrs.ability n) attrs #resource 1
      doStep 1 msg
      pure s
    -- "Forced - If there are 1 [per_investigator] or more resources on this
    -- card: Flip it and move it to the victory display." Abilities 2 and 3 are
    -- the only things that put resources here, so the check rides on them.
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) _) -> do
      required <- perPlayer 1
      if attrs.token #resource >= required
        then do
          addToVictory iid attrs
          pure . CautiousJailers $ attrs & flippedL .~ True
        else pure s
    _ -> CautiousJailers <$> liftRunMessage msg attrs
