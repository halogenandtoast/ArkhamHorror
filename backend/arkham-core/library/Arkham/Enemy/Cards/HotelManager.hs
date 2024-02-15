module Arkham.Enemy.Cards.HotelManager (
  hotelManager,
  HotelManager (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Trait (Trait (Guest))

newtype HotelManager = HotelManager EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hotelManager :: EnemyCard HotelManager
hotelManager = enemy HotelManager Cards.hotelManager (3, PerPlayer 6, 4) (2, 2)

instance HasModifiersFor HotelManager where
  getModifiersFor (EnemyTarget eid) (HotelManager attrs) = do
    isGuest <- eid <=~> EnemyWithTrait Guest
    pure $ toModifiers attrs $ guard isGuest *> [LosePatrol, AddKeyword Keyword.Surge]
  getModifiersFor _ _ = pure []

instance HasAbilities HotelManager where
  getAbilities (HotelManager a) =
    withBaseAbilities
      a
      [restrictedAbility a 1 (exists $ EnemyWithTrait Guest) $ ForcedAbility $ PhaseBegins #when #enemy]

instance RunMessage HotelManager where
  runMessage msg e@(HotelManager attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      lead <- getLeadPlayer
      edibleGuests <- select $ EnemyAt (locationWithEnemy $ toId attrs) <> EnemyWithTrait Guest
      otherGuests <-
        select $ NotEnemy (EnemyAt $ locationWithEnemy $ toId attrs) <> EnemyWithTrait Guest
      pushAll
        $ ( guard (notNull edibleGuests)
              *> [ chooseOrRunOne
                    lead
                    [ targetLabel
                      guest
                      [AddToVictory (toTarget guest), PlaceDoom (toAbilitySource attrs 1) (toTarget attrs) 2]
                    | guest <- edibleGuests
                    ]
                 ]
          )
        <> ( guard (notNull otherGuests)
              *> [MoveToward (toTarget guest) $ locationWithEnemy $ toId attrs | guest <- otherGuests]
           )

      pure e
    _ -> HotelManager <$> runMessage msg attrs
