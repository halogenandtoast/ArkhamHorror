module Arkham.Homebrew.NightOfVespers.Locations.HotelLobby_020 (hotelLobby_020) where

import Arkham.Ability
import Arkham.Card (cardMatch, card_)
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Scenario (scenarioField)
import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenario.Types (Field (ScenarioDiscard))
import Arkham.Trait (Trait (Hazard))

newtype HotelLobby_020 = HotelLobby_020 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hotelLobby_020 :: LocationCard HotelLobby_020
hotelLobby_020 = symbolLabel $ location HotelLobby_020 Cards.hotelLobby_020 3 (PerPlayer 2)

instance HasModifiersFor HotelLobby_020 where
  getModifiersFor (HotelLobby_020 a) = whenRevealed a do
    modifySelect
      a
      (investigatorAt a)
      [ActionSkillModifier #fight #combat 2, ActionSkillModifier #evade #agility 2]

-- Printed on the unrevealed side but offered on both, because the window resolves
-- after this location is revealed. It rides the after-move window so that the
-- act's after-enter removal of unrevealed locations resolves first.
instance HasAbilities HotelLobby_020 where
  getAbilities (HotelLobby_020 a) =
    extend a
      $ mkAbility a 1 (forced $ Moves #after You AnySource Anywhere (be a))
      : (guard a.revealed *> [mkAbility a 2 $ forced $ RevealLocation #when You (be a)])

instance RunMessage HotelLobby_020 where
  runMessage msg l@(HotelLobby_020 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeSetAsideLocationsMatching_
        $ mapOneOf
          cardIs
          [ Cards.hedgeMaze
          , Cards.conservatory
          ]
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      discards <- scenarioField ScenarioDiscard
      for_ (find (`cardMatch` card_ (#treachery <> withTrait Hazard)) discards)
        $ drawCardFrom iid Deck.EncounterDiscard
      pure l
    _ -> HotelLobby_020 <$> liftRunMessage msg attrs
