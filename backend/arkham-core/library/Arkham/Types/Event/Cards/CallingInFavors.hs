module Arkham.Types.Event.Cards.CallingInFavors
  ( callingInFavors
  , CallingInFavors(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Card
import Arkham.Types.Card.Cost
import Arkham.Types.Classes
import Arkham.Types.EffectMetadata
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Helpers
import Arkham.Types.Event.Runner
import Arkham.Types.Matcher hiding (PlayCard)
import Arkham.Types.Message
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Trait
import Arkham.Types.Window (Window(..))
import qualified Arkham.Types.Window as Window

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
              , SearchTopOfDeck
                iid
                (toSource attrs)
                (InvestigatorTarget iid)
                9
                []
                (DeferAllSearchedToTarget $ toTarget attrs)
              ]
          | (target, cost) <- targetsWithCosts
          ]
      e <$ pushAll [choice, Discard (toTarget attrs)]
    SearchTopOfDeckAll iid target _ cards | isTarget attrs target -> do
      let
        windows' =
          [ Window Timing.When Window.NonFast
          , Window Timing.When (Window.DuringTurn iid)
          ]
      playableCards <-
        filterM (getIsPlayable iid (toSource attrs) windows') $ filter
          (`cardMatch` (CardWithType AssetType <> CardWithTrait Ally))
          cards
      if null playableCards
        then e <$ pushAll
          [ FocusCards cards
          , chooseOne iid [Label "No playable allies found" []]
          , ShuffleCardsIntoDeck iid (mapMaybe (preview _PlayerCard) cards)
          , UnfocusCards
          ]
        else e <$ pushAll
          [ FocusCards cards
          , chooseOne
            iid
            [ TargetLabel
                (CardIdTarget $ toCardId card)
                [ AddToHand iid card
                , ShuffleCardsIntoDeck
                  iid
                  (mapMaybe (preview _PlayerCard) $ filter (/= card) cards)
                , UnfocusCards
                , PlayCard iid (toCardId card) Nothing False
                ]
            | card <- playableCards
            ]
          ]
    _ -> CallingInFavors <$> runMessage msg attrs
