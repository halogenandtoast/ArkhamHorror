module Arkham.Types.Event.Cards.LogicalReasoning
  ( logicalReasoning
  , LogicalReasoning(..)
  ) where

import Arkham.Prelude hiding (terror)

import Arkham.Event.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Trait

newtype LogicalReasoning = LogicalReasoning EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

logicalReasoning :: EventCard LogicalReasoning
logicalReasoning = event LogicalReasoning Cards.logicalReasoning

instance EventRunner env => RunMessage env LogicalReasoning where
  runMessage msg e@(LogicalReasoning attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      iids <- selectList $ InvestigatorAt YourLocation
      options <- for iids $ \iid' -> do
        hasHorror <- member iid' <$> select InvestigatorWithAnyHorror
        terrors <-
          selectList $ TreacheryWithTrait Terror <> TreacheryInThreatAreaOf
            (InvestigatorWithId iid')
        pure
          ( iid'
          , [ Label "Heal 2 Horror" [HealHorror (InvestigatorTarget iid') 2]
            | hasHorror
            ]
          <> [ Label
                 "Discard a Terror"
                 [ chooseOne
                     iid'
                     [ Discard (TreacheryTarget terror) | terror <- terrors ]
                 ]
             | notNull terrors
             ]
          )
      let
        choices = map
          (\(iid', choices) -> TargetLabel
            (InvestigatorTarget iid')
            [chooseOrRunOne iid' choices]
          )
          options
      e <$ pushAll [chooseOrRunOne iid choices, Discard (toTarget attrs)]
    _ -> LogicalReasoning <$> runMessage msg attrs
