module Arkham.Asset.Cards.KeyOfYs
  ( keyOfYs
  , KeyOfYs(..)
  )
where

import Arkham.Prelude

import Arkham.Ability
import qualified Arkham.Asset.Cards as Cards
import Arkham.Asset.Runner
import Arkham.Criteria
import Arkham.Matcher
import Arkham.GameValue
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype KeyOfYs = KeyOfYs AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

keyOfYs :: AssetCard KeyOfYs
keyOfYs =
  assetWith KeyOfYs Cards.keyOfYs (sanityL ?~ 4)

instance HasModifiersFor KeyOfYs where
  getModifiersFor (InvestigatorTarget iid) (KeyOfYs a) = 
    pure [ toModifier a (AnySkillValue $ assetHorror a) | controlledBy a iid ]
  getModifiersFor _ _ = pure []

instance HasAbilities KeyOfYs where
  getAbilities (KeyOfYs x) =
    [ restrictedAbility x 1 ControlsThis
        $ ForcedAbility
        $ PlacedCounter Timing.When You HorrorCounter (AtLeast $ Static 1)
    , restrictedAbility x 2 ControlsThis
        $ ForcedAbility
        $ AssetLeavesPlay Timing.When
        $ AssetWithId
        $ toId x
    ]

instance RunMessage KeyOfYs where
  runMessage msg a@(KeyOfYs attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ MovedHorror source (InvestigatorTarget iid) 1
      pure . KeyOfYs $ attrs & horrorL +~ 1
    UseCardAbility iid source 2 _ _ | isSource attrs source -> do
      push $ DiscardTopOfDeck iid 10 Nothing
      pure a
    _ -> KeyOfYs <$> runMessage msg attrs
