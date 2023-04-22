module Arkham.Location.Cards.WalterGilmansRoom (
  walterGilmansRoom,
  WalterGilmansRoom (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Helpers.Investigator
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Runner
import Arkham.Matcher

newtype WalterGilmansRoom = WalterGilmansRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

walterGilmansRoom :: LocationCard WalterGilmansRoom
walterGilmansRoom =
  locationWith
    WalterGilmansRoom
    Cards.walterGilmansRoom
    4
    (PerPlayer 1)
    ( costToEnterUnrevealedL
        .~ Costs
          [ActionCost 1, GroupClueCost (PerPlayer 1) (locationIs Locations.moldyHalls)]
    )

instance HasAbilities WalterGilmansRoom where
  getAbilities (WalterGilmansRoom a) =
    withRevealedAbilities
      a
      [ restrictedAbility a 1 Here $ ActionAbility Nothing $ ActionCost 1
      , haunted "Discard the top 2 cards of the encounter deck." a 2
      ]

instance RunMessage WalterGilmansRoom where
  runMessage msg l@(WalterGilmansRoom attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      canDraw <- getCanDrawCards iid
      drawing <- drawCards iid attrs 3
      pushAll $
        [drawing | canDraw]
          <> [InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 1]
      pure l
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      push $ DiscardTopOfEncounterDeck iid 2 (toSource attrs) Nothing
      pure l
    _ -> WalterGilmansRoom <$> runMessage msg attrs
