module Arkham.Asset.Cards.AdamLynch
  ( adamLynch
  , AdamLynch(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Token

newtype AdamLynch = AdamLynch AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

adamLynch :: AssetCard AdamLynch
adamLynch =
  allyWith AdamLynch Cards.adamLynch (1, 1)
    $ (isStoryL .~ True)
    . (slotsL .~ mempty)

instance HasAbilities AdamLynch where
  getAbilities (AdamLynch x) =
    [ mkAbility x 1
        $ ForcedAbility
        $ AssetLeavesPlay Timing.When
        $ AssetWithId
        $ toId x
    ]

instance HasModifiersFor AdamLynch where
  getModifiersFor (InvestigatorTarget iid) (AdamLynch attrs)
    | controlledBy attrs iid = do
      mSecurityOffice <- selectOne (LocationWithTitle "Security Office")
      case mSecurityOffice of
        Just securityOffice -> pure $ toModifiers
          attrs
          [ AbilityModifier
              (LocationTarget securityOffice)
              1
              (ActionCostSetToModifier 1)
          ]
        Nothing -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage AdamLynch where
  runMessage msg a@(AdamLynch attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source ->
      a <$ pushAll [AddToken Tablet, RemoveFromGame $ toTarget attrs]
    _ -> AdamLynch <$> runMessage msg attrs
