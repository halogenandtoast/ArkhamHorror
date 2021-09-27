module Arkham.Types.Asset.Cards.PeterClover
  ( peterClover
  , PeterClover(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Matcher hiding (EnemyEvaded)
import Arkham.Types.Message
import Arkham.Types.Phase
import Arkham.Types.Timing qualified as Timing
import Arkham.Types.Trait

newtype PeterClover = PeterClover AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

peterClover :: AssetCard PeterClover
peterClover =
  allyWith PeterClover Cards.peterClover (3, 2)
    $ (slotsL .~ [])
    . (isStoryL .~ True)

instance HasAbilities PeterClover where
  getAbilities (PeterClover x) =
    [ restrictedAbility x 1 Unowned
    $ ForcedAbility
    $ PhaseBegins Timing.When
    $ PhaseIs EnemyPhase
    , restrictedAbility
      x
      2
      (OwnsThis <> EnemyCriteria
        (EnemyExists $ EnemyAt YourLocation <> EnemyWithTrait Criminal)
      )
      (FastAbility $ ExhaustCost $ toTarget x)
    ]

instance AssetRunner env => RunMessage env PeterClover where
  runMessage msg a@(PeterClover attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      a <$ push (AssetDamage (toId attrs) source 1 0)
    UseCardAbility iid source _ 2 _ | isSource attrs source -> do
      criminals <- selectList $ EnemyWithTrait Criminal <> EnemyAt YourLocation
      a <$ push (chooseOne iid [ EnemyEvaded iid eid | eid <- criminals ])
    _ -> PeterClover <$> runMessage msg attrs
