module Arkham.Event.Cards.LogicalReasoning
  ( logicalReasoning
  , LogicalReasoning(..)
  ) where

import Arkham.Prelude hiding ( terror )

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Message
import Arkham.Target
import Arkham.Trait

newtype LogicalReasoning = LogicalReasoning EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

logicalReasoning :: EventCard LogicalReasoning
logicalReasoning = event LogicalReasoning Cards.logicalReasoning

instance RunMessage LogicalReasoning where
  runMessage msg e@(LogicalReasoning attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      iids <- selectList $ InvestigatorAt YourLocation
      options <- for iids $ \iid' -> do
        mHealHorror <- getHealHorrorMessage attrs 2 iid'
        terrors <-
          selectList $ TreacheryWithTrait Terror <> TreacheryInThreatAreaOf
            (InvestigatorWithId iid')
        pure
          ( iid'
          , [ Label "Heal 2 Horror" [healHorror]
            | healHorror <- maybeToList mHealHorror
            ]
          <> [ Label
                 "Discard a Terror"
                 [ chooseOne
                     iid'
                     [ targetLabel terror [Discard (toSource attrs) $ TreacheryTarget terror]
                     | terror <- terrors
                     ]
                 ]
             | notNull terrors
             ]
          )
      let
        choices = map
          (\(iid', choices') -> TargetLabel
            (InvestigatorTarget iid')
            [chooseOrRunOne iid' choices']
          )
          options
      e <$ pushAll [chooseOrRunOne iid choices]
    _ -> LogicalReasoning <$> runMessage msg attrs
