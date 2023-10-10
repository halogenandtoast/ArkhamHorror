module Arkham.Location.Cards.JoeMazurewiczsRoom (
  joeMazurewiczsRoom,
  JoeMazurewiczsRoom (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Trait (Trait (Blessed, Item))

newtype JoeMazurewiczsRoom = JoeMazurewiczsRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

joeMazurewiczsRoom :: LocationCard JoeMazurewiczsRoom
joeMazurewiczsRoom =
  location JoeMazurewiczsRoom Cards.joeMazurewiczsRoom 3 (PerPlayer 1)

instance HasAbilities JoeMazurewiczsRoom where
  getAbilities (JoeMazurewiczsRoom a) =
    withBaseAbilities
      a
      [ limitedAbility (GroupLimit PerGame 1)
          $ restrictedAbility a 1 Here
          $ ActionAbility Nothing
          $ ActionCost 1
      , haunted
          "You must either take 1 horror, or choose and discard an asset you control."
          a
          2
      ]

instance RunMessage JoeMazurewiczsRoom where
  runMessage msg l@(JoeMazurewiczsRoom attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push
        $ search
          iid
          (toSource attrs)
          (InvestigatorTarget iid)
          [fromDeck]
          (CardWithOneOf [CardWithTrait Blessed, CardWithTrait Item])
          (DrawFound iid 1)
      pure l
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      hasAssets <- selectAny $ DiscardableAsset <> assetControlledBy iid
      player <- getPlayer iid
      push
        $ chooseOrRunOne player
        $ Label
          "Take 1 horror"
          [InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 1]
        : [ Label
            "Choose and discard an asset you control"
            [ChooseAndDiscardAsset iid (toSource attrs) AnyAsset]
          | hasAssets
          ]
      pure l
    _ -> JoeMazurewiczsRoom <$> runMessage msg attrs
