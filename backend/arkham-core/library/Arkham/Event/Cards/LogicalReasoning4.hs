module Arkham.Event.Cards.LogicalReasoning4 (
  logicalReasoning4,
  LogicalReasoning4 (..),
) where

import Arkham.Prelude hiding (terror)

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Investigator
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Trait

newtype LogicalReasoning4 = LogicalReasoning4 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

logicalReasoning4 :: EventCard LogicalReasoning4
logicalReasoning4 = event LogicalReasoning4 Cards.logicalReasoning4

instance RunMessage LogicalReasoning4 where
  runMessage msg e@(LogicalReasoning4 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      n <- fieldMap InvestigatorClues (min 3) iid
      pushAll $ replicate n $ ResolveEvent iid eid Nothing []
      pure e
    ResolveEvent iid eid _ _ | eid == toId attrs -> do
      iids <- selectList $ affectsOthers $ colocatedWith iid
      options <- for iids $ \iid' -> do
        mHealHorror <- getHealHorrorMessage attrs 2 iid'
        terrors <- selectList $ TreacheryWithTrait Terror <> treacheryInThreatAreaOf iid'
        player <- getPlayer iid'
        pure
          ( iid'
          , player
          , [Label "Heal 2 Horror" [healHorror] | healHorror <- toList mHealHorror]
              <> [ Label
                  "Discard a Terror"
                  [ chooseOne player
                      $ targetLabels terrors
                      $ only
                      . toDiscardBy iid attrs
                  ]
                 | notNull terrors
                 ]
          )
      let
        choices = flip mapMaybe options $ \(iid', player, choices') ->
          case choices' of
            [] -> Nothing
            _ -> Just $ targetLabel iid' [chooseOrRunOne player choices']
      player <- getPlayer iid
      pushWhen (notNull choices) $ chooseOrRunOne player choices
      pure e
    _ -> LogicalReasoning4 <$> runMessage msg attrs
