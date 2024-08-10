module Arkham.Asset.Cards.AdamLynch (
  adamLynch,
  AdamLynch (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.ChaosToken
import Arkham.Matcher

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
    [forcedAbility x 1 $ AssetLeavesPlay #when $ AssetWithId $ toId x]

instance HasModifiersFor AdamLynch where
  getModifiersFor (AbilityTarget iid ab) (AdamLynch attrs) | controlledBy attrs iid = do
    mSecurityOffice <- selectOne (LocationWithTitle "Security Office")
    pure
      $ toModifiers
        attrs
        [ ActionCostSetToModifier 1
        | securityOffice <- toList mSecurityOffice
        , ab.source.location == Just securityOffice
        ]
  getModifiersFor _ _ = pure []

instance RunMessage AdamLynch where
  runMessage msg a@(AdamLynch attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      pushAll [AddChaosToken Tablet, RemoveFromGame $ toTarget attrs]
      pure a
    _ -> AdamLynch <$> runMessage msg attrs
