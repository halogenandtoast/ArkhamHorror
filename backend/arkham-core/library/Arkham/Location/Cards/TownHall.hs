module Arkham.Location.Cards.TownHall (
  townHall,
  TownHall (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Investigator.Types (Field (InvestigatorHand))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Projection
import Arkham.Timing qualified as Timing

newtype TownHall = TownHall LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

townHall :: LocationCard TownHall
townHall = location TownHall Cards.townHall 4 (PerPlayer 1)

instance HasModifiersFor TownHall where
  getModifiersFor (LocationTarget lid) (TownHall a) = do
    isDowntown <- lid <=~> locationIs Cards.downtownFirstBankOfArkham
    pure $
      toModifiers
        a
        [ ConnectedToWhen (LocationWithId lid) (LocationWithId $ toId a)
        | isDowntown
        ]
  getModifiersFor _ _ = pure []

instance HasAbilities TownHall where
  getAbilities (TownHall a) =
    withBaseAbilities a $
      [ mkAbility a 1 $
          ForcedAbility $
            Enters Timing.After You $
              LocationWithId $
                toId a
      ]

instance RunMessage TownHall where
  runMessage msg l@(TownHall attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      hand <- field InvestigatorHand iid
      let
        weaknessCount = count cardIsWeakness hand
        discardCount = min (length hand - 3) (length hand - weaknessCount)
      pushAll $
        replicate discardCount $
          toMessage $
            chooseAndDiscardCard
              iid
              (toAbilitySource attrs 1)
      pure l
    _ -> TownHall <$> runMessage msg attrs
