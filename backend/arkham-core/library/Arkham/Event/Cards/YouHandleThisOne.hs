module Arkham.Event.Cards.YouHandleThisOne (
  youHandleThisOne,
  YouHandleThisOne (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher

newtype YouHandleThisOne = YouHandleThisOne EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

youHandleThisOne :: EventCard YouHandleThisOne
youHandleThisOne = event YouHandleThisOne Cards.youHandleThisOne

instance RunMessage YouHandleThisOne where
  runMessage msg e@(YouHandleThisOne attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      targets <-
        selectListMap InvestigatorTarget
          $ NotInvestigator (InvestigatorWithId iid)
      player <- getPlayer iid
      pushAll
        $ [ chooseOrRunOne
              player
              [ TargetLabel
                target
                [HandleTargetChoice iid (toSource attrs) target]
              | target <- targets
              ]
          ]
        <> [TakeResources iid 1 (toSource attrs) False]
      pure e
    HandleTargetChoice iid source (InvestigatorTarget iid')
      | isSource attrs source -> do
          replaceMessageMatching
            ( \case
                Revelation me _ -> me == iid
                InvestigatorDrawEnemy me _ -> me == iid
                _ -> False
            )
            ( \case
                Revelation _ source' -> [Revelation iid' source']
                InvestigatorDrawEnemy _ eid -> [InvestigatorDrawEnemy iid' eid]
                _ -> error "wrong message found"
            )
          pure e
    _ -> YouHandleThisOne <$> runMessage msg attrs
