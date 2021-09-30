module Arkham.Types.Event.Cards.CallingInFavors
  ( callingInFavors
  , CallingInFavors(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Types.Card
import Arkham.Types.Card.Cost
import Arkham.Types.Classes
import Arkham.Types.EffectMetadata
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.Matcher hiding (PlayCard)
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Trait

newtype CallingInFavors = CallingInFavors EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

callingInFavors :: EventCard CallingInFavors
callingInFavors = event CallingInFavors Cards.callingInFavors

instance EventRunner env => RunMessage env CallingInFavors where
  runMessage msg e@(CallingInFavors attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ | eid == toId attrs -> do
      allies <- selectList $ AssetWithTrait Ally <> AssetOwnedBy
        (InvestigatorWithId iid)
      targetsWithCosts <- for
        allies
        \ally -> do
          cardDef <- getCardDef ally
          pure (AssetTarget ally, maybe 0 toPrintedCost $ cdCost cardDef)
      let
        choice = chooseOne
          iid
          [ TargetLabel
              target
              [ ReturnToHand iid target
              , CreateEffect
                (toCardCode attrs)
                (Just $ EffectInt cost)
                (toSource attrs)
                (InvestigatorTarget iid)
              , Search
                iid
                (toSource attrs)
                (InvestigatorTarget iid)
                [fromTopOfDeck 9]
                IsAlly
                (PlayFound iid 1)
              ]
          | (target, cost) <- targetsWithCosts
          ]
      e <$ pushAll [choice, Discard (toTarget attrs)]
    _ -> CallingInFavors <$> runMessage msg attrs
