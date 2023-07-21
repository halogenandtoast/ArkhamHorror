module Arkham.Act.Cards.TheCaveOfDarknessTunnelsInTheDark (
  TheCaveOfDarknessTunnelsInTheDark (..),
  theCaveOfDarknessTunnelsInTheDark,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message
import Arkham.Name
import Arkham.ScenarioLogKey

newtype TheCaveOfDarknessTunnelsInTheDark = TheCaveOfDarknessTunnelsInTheDark ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCaveOfDarknessTunnelsInTheDark :: ActCard TheCaveOfDarknessTunnelsInTheDark
theCaveOfDarknessTunnelsInTheDark =
  act
    (2, E)
    TheCaveOfDarknessTunnelsInTheDark
    Cards.theCaveOfDarknessTunnelsInTheDark
    Nothing

instance HasAbilities TheCaveOfDarknessTunnelsInTheDark where
  getAbilities (TheCaveOfDarknessTunnelsInTheDark attrs) =
    [ restrictedAbility
        attrs
        999
        ( LocationExists $
            LocationWithTitle "Black Cave"
              <> LocationWithoutClues
        )
        $ Objective
        $ FastAbility
        $ GroupClueCost (PerPlayer 2)
        $ LocationWithTitle "Black Cave"
    ]

instance RunMessage TheCaveOfDarknessTunnelsInTheDark where
  runMessage msg a@(TheCaveOfDarknessTunnelsInTheDark attrs) = case msg of
    AdvanceAct aid _ _ | aid == actId attrs && onSide F attrs -> do
      blackCave <- selectJust $ locationIs Locations.blackCave
      mTownHall <- selectOne $ locationIs Locations.townHall
      townHallMessages <- case mTownHall of
        Just townHall ->
          pure
            [ Remember $ IchtacasDestination $ labeled Locations.townHall townHall
            , AddDirectConnection blackCave townHall
            , AddDirectConnection townHall blackCave
            ]
        Nothing -> do
          (townHallId, placeTownHall) <- placeSetAsideLocation Locations.townHall
          pure
            [ placeTownHall
            , Remember $ IchtacasDestination $ labeled Locations.townHall townHallId
            , AddDirectConnection blackCave townHallId
            , AddDirectConnection townHallId blackCave
            ]
      iids <- getInvestigatorIds
      pushAll $
        townHallMessages
          <> [DiscardTopOfEncounterDeck iid 1 (toSource attrs) (Just $ toTarget attrs) | iid <- iids]
          <> [ AdvanceToAct
                (actDeckId attrs)
                Acts.strangeOccurences
                E
                (toSource attrs)
             ]
      pure a
    DiscardedTopOfEncounterDeck iid [card] _ target | isTarget attrs target -> do
      when (toCardType card == TreacheryType) $ do
        push $ InvestigatorDrewEncounterCard iid card
      pure a
    _ -> TheCaveOfDarknessTunnelsInTheDark <$> runMessage msg attrs
