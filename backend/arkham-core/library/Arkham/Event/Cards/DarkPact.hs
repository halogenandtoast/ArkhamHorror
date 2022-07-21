module Arkham.Event.Cards.DarkPact
  ( darkPact
  , DarkPact(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Message

newtype DarkPact = DarkPact EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkPact :: EventCard DarkPact
darkPact = event DarkPact Cards.darkPact

instance HasAbilities DarkMemory where
  getAbilities (DarkMemory x) =
    [ restrictedAbility x 1 InYourHand $ ForcedAbility $ OrWindowMatcher
        [ GameEnds Timing.When
        , InvestigatorEliminated Timing.When (InvestigatorWithId iid)
        ]
    ]

instance RunMessage DarkPact where
  runMessage msg e@(DarkPact attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      iids <- selectList $ colocatedWith iid
      pushAll
        [ chooseOne
          iid
          [ targetLabel
              iid'
              [InvestigatorAssignDamage iid' (toSource attrs) DamageAny 2 0]
          | iid' <- iids
          ]
        , Discard (toTarget attrs)
        ]
      pure e
    InHand iid' (UseCardAbility iid (isSource attrs -> True) _ 1 _)
      | iid' == iid -> case toCard attrs of
          EncounterCard _ -> error "should be player card"
          PlayerCard pc -> do
            thePriceOfFailure <- genPlayerCard Treacheries.thePriceOfFailure
            pushAll
              [ RemoveCardFromDeckForCampaign iid $ toCard attrs
              , AddCardToDeckForCampaign iid thePriceOfFailure
              ]
            pure e
    _ -> DarkPact <$> runMessage msg attrs
