module Arkham.Location.Cards.GrandEntryway (grandEntryway, GrandEntryway (..)) where

import Arkham.Ability
import Arkham.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers (resignAction)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier
import Arkham.ScenarioLogKey
import Arkham.Trait (Trait (Cave))

newtype GrandEntryway = GrandEntryway LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grandEntryway :: LocationCard GrandEntryway
grandEntryway = location GrandEntryway Cards.grandEntryway 1 (Static 0)

instance HasModifiersFor GrandEntryway where
  getModifiersFor (LocationTarget lid) (GrandEntryway attrs) = do
    toModifiers attrs
      $ if lid == attrs.id
        then [ConnectedToWhen (LocationWithId attrs.id) (LocationWithTrait Cave)]
        else [ConnectedToWhen (LocationWithTrait Cave) (LocationWithId attrs.id)]
  getModifiersFor _ _ = pure []

instance HasAbilities GrandEntryway where
  getAbilities (GrandEntryway attrs) =
    extendRevealed
      attrs
      [ withTooltip "You've had enough of this place." $ resignAction attrs
      , restricted attrs 1 Here
          $ actionAbilityWithCost
          $ GroupSpendKeyCost PurpleKey (be attrs)
          <> GroupClueCost (PerPlayer 3) (be attrs)
      ]

instance RunMessage GrandEntryway where
  runMessage msg l@(GrandEntryway attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      remember UnlockedTheEntranceToTheCaves
      pure l
    _ -> GrandEntryway <$> liftRunMessage msg attrs
