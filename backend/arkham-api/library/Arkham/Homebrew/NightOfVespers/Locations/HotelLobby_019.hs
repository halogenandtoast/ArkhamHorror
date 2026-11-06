module Arkham.Homebrew.NightOfVespers.Locations.HotelLobby_019 (hotelLobby_019) where

import Arkham.Ability
import Arkham.Deck qualified as Deck
import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Trait (Trait (Syndicate))

newtype HotelLobby_019 = HotelLobby_019 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hotelLobby_019 :: LocationCard HotelLobby_019
hotelLobby_019 = symbolLabel $ location HotelLobby_019 Cards.hotelLobby_019 3 (PerPlayer 2)

-- Printed on the unrevealed side but offered on both, because the window resolves
-- after this location is revealed. It rides the after-move window so that the
-- act's after-enter removal of unrevealed locations resolves first.
instance HasAbilities HotelLobby_019 where
  getAbilities (HotelLobby_019 a) =
    extend a
      $ mkAbility a 1 (forced $ Moves #after You AnySource Anywhere (be a))
      : ( guard a.revealed
            *> [ mkAbility a 2 $ forced $ RevealLocation #when You (be a)
               , evadeAbility a 3 mempty Here
               ]
        )

overlookedEnemies :: EnemyMatcher
overlookedEnemies =
  EnemyAt $ mapOneOf LocationWithTitle ["Hotel Wine Cellar", "Galerie Lavandi\232re"]

instance RunMessage HotelLobby_019 where
  runMessage msg l@(HotelLobby_019 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeSetAsideLocationsMatching_
        $ mapOneOf
          cardIs
          [ Cards.hedgeMaze
          , Cards.conservatory
          ]
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      discardUntilFirst iid (attrs.ability 2) Deck.EncounterDeck (basic $ #enemy <> withTrait Syndicate)
      pure l
    RequestedEncounterCard (isAbilitySource attrs 2 -> True) _ (Just card) -> do
      createEnemyAtLocationMatching_ card (LocationWithTitle "Trattoria del Parche")
      pure l
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      sid <- getRandom
      chooseEvadeEnemyMatch sid iid (attrs.ability 3) (oneOf [enemyAt attrs, overlookedEnemies])
      pure l
    ChosenEvadeEnemy sid (isAbilitySource attrs 3 -> True) enemy -> do
      whenM (enemy <=~> overlookedEnemies)
        $ skillTestModifier sid (attrs.ability 3) sid (Difficulty 1)
      pure l
    _ -> HotelLobby_019 <$> liftRunMessage msg attrs
