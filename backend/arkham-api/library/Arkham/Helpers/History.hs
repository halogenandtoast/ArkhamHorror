module Arkham.Helpers.History (module Arkham.Helpers.History, module X) where

import Arkham.Card (cardMatch, lookupCardDef, toCardDef)
import Arkham.Card.CardDef (cdCardTraits)
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.Enemy.Types (enemyLastKnownLocation, enemyPlacement)
import Arkham.GameEnv
import Arkham.GameEnv as X (getHistoryField)
import Arkham.Helpers.GameValue (gameValueMatches)
import Arkham.Helpers.Query
import Arkham.History as X
import Arkham.Id
import Arkham.Matcher qualified as Matcher
import Arkham.Placement
import Arkham.SkillTestResult (isFailedResult, isSuccessResult)
import Arkham.Prelude

historyMatches :: HasGame m => Matcher.HistoryMatcher -> History -> m Bool
historyMatches = \case
  Matcher.DefeatedEnemiesWithTotalHealth vMatcher ->
    (`gameValueMatches` vMatcher) . sum . map defeatedEnemyHealth . historyEnemiesDefeated
  Matcher.DefeatedEnemyWithTraitAt trait lid ->
    pure
      . any
        ( \defeated ->
            let attrs = defeatedEnemyAttrs defeated
                mloc = case enemyPlacement attrs of
                  AtLocation lid' -> Just lid'
                  _ -> enemyLastKnownLocation attrs
             in trait `member` cdCardTraits (toCardDef attrs) && mloc == Just lid
        )
      . historyEnemiesDefeated
  Matcher.AttackedByAnyEnemies -> pure . notNull . historyEnemiesAttackedBy
  Matcher.AttackedByEnemy eid -> pure . elem eid . historyEnemiesAttackedBy
  Matcher.CluesDiscoveredAt vMatcher lid ->
    (`gameValueMatches` vMatcher) . findWithDefault 0 lid . historyCluesDiscovered
  Matcher.CluesDiscovered vMatcher -> countIs vMatcher (sum . historyCluesDiscovered)
  Matcher.DrewTreacheries vMatcher -> countIs vMatcher (length . historyTreacheriesDrawn)
  Matcher.DrewEnemies vMatcher -> countIs vMatcher (length . historyEnemiesDrawn)
  Matcher.DrewTreacheryWithTrait t -> pure . anyWithTrait t . historyTreacheriesDrawn
  Matcher.DrewEnemyWithTrait t -> pure . anyWithTrait t . historyEnemiesDrawn
  Matcher.PlayedCardsMatching vMatcher matcher ->
    countIs vMatcher (length . filter ((`cardMatch` matcher) . playedCard) . historyPlayedCards)
  Matcher.SkillTestsPerformed vMatcher -> countIs vMatcher (length . historySkillTestsPerformed)
  Matcher.SuccessfulSkillTests vMatcher ->
    countIs vMatcher (length . filter (isSuccessResult . snd) . historySkillTestsPerformed)
  Matcher.FailedSkillTests vMatcher ->
    countIs vMatcher (length . filter (isFailedResult . snd) . historySkillTestsPerformed)
  Matcher.ActionsCompleted vMatcher -> countIs vMatcher historyActionsCompleted
  Matcher.ActionsSpent vMatcher -> countIs vMatcher historyActionsSpent
  Matcher.TimesMoved vMatcher -> countIs vMatcher historyMoved
  Matcher.MovedToLocation matcher -> \h -> do
    wanted <- select matcher
    pure $ any (`elem` wanted) (historyMovedTo h)
  Matcher.RevealedLocations vMatcher -> countIs vMatcher (length . historyLocationsRevealed)
  Matcher.ResourcesGained vMatcher -> countIs vMatcher historyResourcesGained
  Matcher.CardsDrawn vMatcher -> countIs vMatcher historyCardsDrawn
  Matcher.SuccessfulAttacks vMatcher -> countIs vMatcher historySuccessfulAttacks
  Matcher.SuccessfulEvasions vMatcher -> countIs vMatcher historySuccessfulEvasions
  Matcher.SuccessfulInvestigations vMatcher -> countIs vMatcher historySuccessfulInvestigations
  Matcher.AttacksOfOpportunityTaken vMatcher -> countIs vMatcher historyAttacksOfOpportunity
  Matcher.SuccessfullyInvestigated matcher -> \h -> do
    wanted <- select matcher
    pure $ any (`elem` wanted) (historyLocationsSuccessfullyInvestigated h)
  Matcher.SuccessfullyExplored -> pure . historySuccessfulExplore
  Matcher.DealtDamageToAnything -> pure . notNull . historyDealtDamageTo
  Matcher.HistoryOneOf matchers -> \h -> anyM (`historyMatches` h) matchers
  Matcher.HistoryAll matchers -> \h -> allM (`historyMatches` h) matchers
 where
  countIs vMatcher f h = gameValueMatches (f h) vMatcher
  {- A card code is all the history keeps, so the trait comes off the def it
  looks up. An unknown code is no match rather than an error: a code with no def
  cannot have the trait. -}
  anyWithTrait t = any \code ->
    maybe False ((t `member`) . cdCardTraits) (lookupCardDef code)

getAllHistoryField :: (HasGame m, Monoid k) => HistoryType -> HistoryField k -> m k
getAllHistoryField htype fld = concatMap (viewHistoryField fld) <$> (traverse (getHistory htype) =<< getInvestigators)

hasHistory
  :: (HasGame m, ToId investigator InvestigatorId)
  => HistoryType -> Matcher.HistoryMatcher -> investigator -> m Bool
hasHistory htype matcher (asId -> iid) = getHistory htype iid >>= historyMatches matcher
