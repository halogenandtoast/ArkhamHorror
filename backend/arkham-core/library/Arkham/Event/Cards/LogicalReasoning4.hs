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
import Arkham.Message
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
      iids <- selectList $ colocatedWith iid
      options <- for iids $ \iid' -> do
        mHealHorror <- getHealHorrorMessage attrs 2 iid'
        terrors <- selectList $ TreacheryWithTrait Terror <> treacheryInThreatAreaOf iid'
        pure
          ( iid'
          , [Label "Heal 2 Horror" [healHorror] | healHorror <- toList mHealHorror]
              <> [ Label "Discard a Terror"
                  $ [ chooseOne iid'
                        $ [ targetLabel terror [Discard (toSource attrs) $ toTarget terror]
                          | terror <- terrors
                          ]
                    ]
                 | notNull terrors
                 ]
          )
      let
        choices = flip mapMaybe options $ \(iid', choices') ->
          case choices' of
            [] -> Nothing
            _ -> Just $ targetLabel iid' [chooseOrRunOne iid' choices']
      pushWhen (notNull choices) $ chooseOrRunOne iid choices
      pure e
    _ -> LogicalReasoning4 <$> runMessage msg attrs
