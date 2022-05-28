module Arkham.Event.Cards.AChanceEncounter2
  ( aChanceEncounter2
  , AChanceEncounter2(..)
  ) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Card.Cost
import Arkham.Classes
import Arkham.Event.Attrs
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Helpers
import Arkham.Id
import Arkham.Message
import Arkham.Modifier
import Arkham.Source
import Arkham.Target
import Arkham.Trait

newtype AChanceEncounter2 = AChanceEncounter2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aChanceEncounter2 :: EventCard AChanceEncounter2
aChanceEncounter2 = event AChanceEncounter2 Cards.aChanceEncounter2

instance
  ( HasModifiersFor env ()
  , HasSet InvestigatorId env ()
  , HasList DiscardedPlayerCard env InvestigatorId
  )
  => RunMessage env AChanceEncounter2 where
  runMessage msg e@(AChanceEncounter2 attrs) = case msg of
    InvestigatorPlayDynamicEvent iid eid payment | eid == toId attrs -> do
      investigatorIds <-
        filterM
            (fmap (notElem CardsCannotLeaveYourDiscardPile)
            . getModifiers GameSource
            . InvestigatorTarget
            )
          =<< getInvestigatorIds
      discards <-
        map PlayerCard
        . concat
        <$> traverse
              (fmap (map unDiscardedPlayerCard) . getList)
              investigatorIds
      let
        filteredDiscards = filter
          (and . sequence
            [ elem Ally . toTraits
            , (== payment) . maybe 0 toPrintedCost . cdCost . toCardDef
            ]
          )
          discards

      -- Normally we would not error like this, but verifying card costs to
      -- match what is paid is quite difficult. The front-end should just not
      -- update the game state if invalid due to erroring
      when (null filteredDiscards) (error "Invalid choice")

      e <$ pushAll
        [ FocusCards filteredDiscards
        , chooseOne
          iid
          [ TargetLabel
              (CardIdTarget $ toCardId card)
              [ PutCardIntoPlay iid card Nothing
              , RemoveFromDiscard iid (toCardId card)
              ]
          | card <- filteredDiscards
          ]
        , UnfocusCards
        , Discard (toTarget attrs)
        ]
    _ -> AChanceEncounter2 <$> runMessage msg attrs
