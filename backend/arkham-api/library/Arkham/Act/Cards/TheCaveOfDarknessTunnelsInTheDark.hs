module Arkham.Act.Cards.TheCaveOfDarknessTunnelsInTheDark (theCaveOfDarknessTunnelsInTheDark) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.Name
import Arkham.ScenarioLogKey

newtype TheCaveOfDarknessTunnelsInTheDark = TheCaveOfDarknessTunnelsInTheDark ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCaveOfDarknessTunnelsInTheDark :: ActCard TheCaveOfDarknessTunnelsInTheDark
theCaveOfDarknessTunnelsInTheDark =
  act (2, E) TheCaveOfDarknessTunnelsInTheDark Cards.theCaveOfDarknessTunnelsInTheDark Nothing

instance HasAbilities TheCaveOfDarknessTunnelsInTheDark where
  getAbilities (TheCaveOfDarknessTunnelsInTheDark attrs) =
    [ restricted attrs 999 (exists $ LocationWithTitle "Black Cave" <> LocationWithoutClues)
        $ Objective
        $ FastAbility
        $ GroupClueCost (PerPlayer 2)
        $ LocationWithTitle "Black Cave"
    ]

instance RunMessage TheCaveOfDarknessTunnelsInTheDark where
  runMessage msg a@(TheCaveOfDarknessTunnelsInTheDark attrs) = runQueueT $ case msg of
    AdvanceAct (isSide F attrs -> True) _ _ -> do
      blackCave <- selectJust $ locationIs Locations.blackCave
      townHall <- selectOrPlaceSetAsideLocation Locations.townHall
      remember $ IchtacasDestination $ labeled Locations.townHall townHall
      push $ AddDirectConnection blackCave townHall
      push $ AddDirectConnection townHall blackCave
      eachInvestigator \iid -> discardTopOfEncounterDeckAndHandle iid attrs 1 attrs
      advanceToAct attrs Acts.strangeOccurences E
      pure a
    DiscardedTopOfEncounterDeck iid [card] _ target | isTarget attrs target -> do
      when (toCardType card == TreacheryType) $ do
        push $ InvestigatorDrewEncounterCard iid card
      pure a
    _ -> TheCaveOfDarknessTunnelsInTheDark <$> liftRunMessage msg attrs
