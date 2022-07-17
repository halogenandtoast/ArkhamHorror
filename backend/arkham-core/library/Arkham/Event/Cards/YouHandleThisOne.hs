module Arkham.Event.Cards.YouHandleThisOne
  ( youHandleThisOne
  , YouHandleThisOne(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Target

newtype YouHandleThisOne = YouHandleThisOne EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

youHandleThisOne :: EventCard YouHandleThisOne
youHandleThisOne = event YouHandleThisOne Cards.youHandleThisOne

instance RunMessage YouHandleThisOne where
  runMessage msg e@(YouHandleThisOne attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      targets <- selectListMap InvestigatorTarget
        $ NotInvestigator (InvestigatorWithId iid)
      pushAll
        $ [ chooseOrRunOne
              iid
              [ TargetLabel
                  target
                  [HandleTargetChoice iid (toSource attrs) target]
              | target <- targets
              ]
          ]
        <> [TakeResources iid 1 False, Discard (toTarget attrs)]
      pure e
    HandleTargetChoice iid source (InvestigatorTarget iid')
      | isSource attrs source -> do
        replaceMessageMatching
          (\case
            Revelation me _ -> me == iid
            _ -> False
          )
          (\case
            Revelation _ source' -> [Revelation iid' source']
            _ -> error "wrong message found"
          )
        pure e
    _ -> YouHandleThisOne <$> runMessage msg attrs
