module Arkham.Location.Cards.BlockedPassage (
  blockedPassage,
  BlockedPassage (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype BlockedPassage = BlockedPassage LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

blockedPassage :: LocationCard BlockedPassage
blockedPassage =
  locationWith
    BlockedPassage
    Cards.blockedPassage
    7
    (Static 0)
    ( (connectsToL .~ adjacentLocations)
        . ( costToEnterUnrevealedL
              .~ Costs [ActionCost 1, GroupClueCost (PerPlayer 1) YourLocation]
          )
    )

instance HasAbilities BlockedPassage where
  getAbilities (BlockedPassage attrs) =
    withBaseAbilities
      attrs
      [ mkAbility attrs 1
        $ ForcedAbility
        $ RevealLocation Timing.When You
        $ LocationWithId
        $ toId attrs
      | locationRevealed attrs
      ]

instance RunMessage BlockedPassage where
  runMessage msg l@(BlockedPassage attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      pushAll
        [ InvestigatorAssignDamage iid (toSource attrs) DamageAny 2 0
        , CreateWindowModifierEffect
            EffectRoundWindow
            (EffectModifiers $ toModifiers attrs [CannotMove])
            (toSource attrs)
            (InvestigatorTarget iid)
        ]
      pure l
    _ -> BlockedPassage <$> runMessage msg attrs
