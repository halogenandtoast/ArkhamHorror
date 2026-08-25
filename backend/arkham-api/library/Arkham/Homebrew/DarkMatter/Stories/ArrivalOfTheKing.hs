module Arkham.Homebrew.DarkMatter.Stories.ArrivalOfTheKing (arrivalOfTheKing) where

import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Homebrew.DarkMatter.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (
  campaignI18n,
  crossOffImpendingDoom,
  crossOffMemories,
  getMemories,
 )
import Arkham.I18n
import Arkham.Investigator.Types (Field (InvestigatorName))
import Arkham.Matcher
import Arkham.Name (toTitle)
import Arkham.Question (AmountTarget (MinAmountTarget))
import Arkham.Story.Import.Lifted

newtype ArrivalOfTheKing = ArrivalOfTheKing StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arrivalOfTheKing :: StoryCard ArrivalOfTheKing
arrivalOfTheKing = story ArrivalOfTheKing Cards.arrivalOfTheKing

{- | "Each investigator may cross out any amount of tally marks next to their
'Memories'. For every 2[per_investigator] tally marks just crossed out, you may
cross out 1 tally mark under 'Impending Doom' in your Campaign Log. Add this
card to the victory display."

Crossing out Impending Doom is never a downside, so the earned reductions are
applied automatically rather than prompted for.
-}
instance RunMessage ArrivalOfTheKing where
  runMessage msg s@(ArrivalOfTheKing attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      named <- selectWithField InvestigatorName UneliminatedInvestigator
      entries <- for named \(iid', name) -> (toTitle name,) <$> getMemories iid'
      let choices = [(name, (0, n)) | (name, n) <- entries, n > 0]
      if null choices
        then addToVictory iid attrs
        else
          campaignI18n
            $ chooseAmounts
              iid
              ("$" <> labelKey "arrivalOfTheKing.memoriesToCrossOut")
              (MinAmountTarget 0)
              choices
              attrs
      pure s
    ResolveAmounts iid choices (isTarget attrs -> True) -> do
      named <- selectWithField InvestigatorName UneliminatedInvestigator
      crossed <- for named \(iid', name) -> do
        let n = getChoiceAmount (toTitle name) choices
        when (n > 0) $ crossOffMemories iid' n
        pure n
      per2 <- perPlayer 2
      when (per2 > 0) $ crossOffImpendingDoom (sum crossed `div` per2)
      addToVictory iid attrs
      pure s
    _ -> ArrivalOfTheKing <$> liftRunMessage msg attrs
