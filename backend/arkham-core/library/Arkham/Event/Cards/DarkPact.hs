module Arkham.Event.Cards.DarkPact (
  darkPact,
  DarkPact (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Cards qualified as Treacheries

newtype DarkPact = DarkPact EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkPact :: EventCard DarkPact
darkPact = event DarkPact Cards.darkPact

instance HasAbilities DarkPact where
  getAbilities (DarkPact x) =
    [ restrictedAbility x 1 InYourHand
        $ ForcedAbility
        $ OrWindowMatcher
          [ GameEnds Timing.When
          , InvestigatorEliminated Timing.When You
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
        ]
      pure e
    InHand iid' (UseCardAbility iid (isSource attrs -> True) 1 _ _)
      | iid' == iid -> case toCard attrs of
          EncounterCard _ -> error "should be player card"
          VengeanceCard _ -> error "should be player card"
          PlayerCard pc -> do
            thePriceOfFailure <- genPlayerCard Treacheries.thePriceOfFailure
            pushAll
              [ RemoveCardFromDeckForCampaign iid pc
              , AddCardToDeckForCampaign iid thePriceOfFailure
              ]
            pure e
    _ -> DarkPact <$> runMessage msg attrs
