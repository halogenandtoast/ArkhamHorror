module Arkham.Location.Cards.AdministrationBuilding where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (administrationBuilding)
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Timing qualified as Timing

newtype AdministrationBuilding = AdministrationBuilding LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

administrationBuilding :: LocationCard AdministrationBuilding
administrationBuilding =
  location AdministrationBuilding Cards.administrationBuilding 4 (PerPlayer 1)

instance HasAbilities AdministrationBuilding where
  getAbilities (AdministrationBuilding x) =
    withRevealedAbilities x $
      [ restrictedAbility x 1 Here $
          ForcedAbility $
            Matcher.RevealLocation Timing.After You $
              LocationWithId $
                toId x
      , restrictedAbility x 2 Here $ ForcedAbility $ TurnEnds Timing.When You
      ]

instance RunMessage AdministrationBuilding where
  runMessage msg l@(AdministrationBuilding attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ PlaceLocationMatching $ CardWithTitle "Faculty Offices"
      pure l
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      push $ DiscardTopOfDeck iid 1 (toAbilitySource attrs 2) Nothing
      pure l
    _ -> AdministrationBuilding <$> runMessage msg attrs
