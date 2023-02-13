module Arkham.Event.Cards.AChanceEncounter2
  ( aChanceEncounter2
  , AChanceEncounter2(..)
  ) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Card.Cost
import Arkham.Classes
import Arkham.Cost
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Helpers
import Arkham.Event.Runner
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Message
import Arkham.Projection
import Arkham.Target
import Arkham.Trait
import Arkham.Window ( defaultWindows )

newtype AChanceEncounter2 = AChanceEncounter2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aChanceEncounter2 :: EventCard AChanceEncounter2
aChanceEncounter2 = event AChanceEncounter2 Cards.aChanceEncounter2

instance RunMessage AChanceEncounter2 where
  runMessage msg e@(AChanceEncounter2 attrs) = case msg of
    PaidForCardCost iid card payment | toCardId card == toCardId attrs -> do
      let resources = totalResourcePayment payment
      investigatorIds <-
        filterM
            (fmap (notElem CardsCannotLeaveYourDiscardPile)
            . getModifiers
            . InvestigatorTarget
            )
          =<< getInvestigatorIds
      discards <-
        concat
          <$> traverse
                (fieldMap InvestigatorDiscard (map PlayerCard))
                investigatorIds
      let
        filteredDiscards = filter
          (and . sequence
            [ elem Ally . toTraits
            , (== resources) . maybe 0 toPrintedCost . cdCost . toCardDef
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
              (CardIdTarget $ toCardId card')
              [ PutCardIntoPlay iid card Nothing (defaultWindows iid)
              , RemoveFromDiscard iid (toCardId card')
              ]
          | card' <- filteredDiscards
          ]
        , UnfocusCards
        ]
    _ -> AChanceEncounter2 <$> runMessage msg attrs
