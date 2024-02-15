module Arkham.Act.Cards.InvestigatingTheWitchHouse (
  InvestigatingTheWitchHouse (..),
  investigatingTheWitchHouse,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Classes
import Arkham.Constants
import Arkham.Deck qualified as Deck
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (DuringTurn)

newtype InvestigatingTheWitchHouse = InvestigatingTheWitchHouse ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

investigatingTheWitchHouse :: ActCard InvestigatingTheWitchHouse
investigatingTheWitchHouse =
  act (1, A) InvestigatingTheWitchHouse Cards.investigatingTheWitchHouse Nothing

instance HasAbilities InvestigatingTheWitchHouse where
  getAbilities (InvestigatingTheWitchHouse a) =
    [ restrictedAbility
        a
        ActAdvancement
        ( DuringTurn Anyone
            <> EachUndefeatedInvestigator (InvestigatorAt $ locationIs Locations.walterGilmansRoom)
        )
        ( Objective
            $ FastAbility
            $ GroupClueCost (PerPlayer 3)
            $ locationIs Locations.walterGilmansRoom
        )
    ]

instance RunMessage InvestigatingTheWitchHouse where
  runMessage msg a@(InvestigatingTheWitchHouse attrs) = case msg of
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      lead <- getLeadPlayer
      iids <- getInvestigatorIds
      lid <- selectJust $ locationIs Locations.walterGilmansRoom
      keziahsRoom <- getSetAsideCard Locations.keziahsRoom
      otherLocations <- select $ NotLocation $ LocationWithId lid
      theBlackBook <- getSetAsideCard Assets.theBlackBook
      strangeGeometries <- getSetAsideCardsMatching (CardWithTitle "Strange Geometry")
      pushAll
        $ ReplaceLocation lid keziahsRoom Swap
        : map RemoveLocation otherLocations
          <> [ chooseOne
                lead
                [ targetLabel
                  iid
                  [TakeControlOfSetAsideAsset iid theBlackBook]
                | iid <- iids
                ]
             , ShuffleCardsIntoDeck Deck.EncounterDeck strangeGeometries
             , advanceActDeck attrs
             ]
      pure a
    _ -> InvestigatingTheWitchHouse <$> runMessage msg attrs
