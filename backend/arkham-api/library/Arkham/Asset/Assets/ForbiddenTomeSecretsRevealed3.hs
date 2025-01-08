module Arkham.Asset.Assets.ForbiddenTomeSecretsRevealed3 (
  forbiddenTomeSecretsRevealed3,
  ForbiddenTomeSecretsRevealed3 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Discover
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Movement

newtype ForbiddenTomeSecretsRevealed3 = ForbiddenTomeSecretsRevealed3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forbiddenTomeSecretsRevealed3 :: AssetCard ForbiddenTomeSecretsRevealed3
forbiddenTomeSecretsRevealed3 = asset ForbiddenTomeSecretsRevealed3 Cards.forbiddenTomeSecretsRevealed3

instance HasModifiersFor ForbiddenTomeSecretsRevealed3 where
  getModifiersFor (ForbiddenTomeSecretsRevealed3 a) = case a.controller of
    Nothing -> pure mempty
    Just iid ->
      selectOne (AbilityIs (toSource a) 1) >>= \case
        Nothing -> pure mempty
        Just ab -> do
          handCount <- getHandCount iid
          let n = handCount `div` 4
          modifiedWhen_ a (n > 0) (AbilityTarget iid ab) [ActionCostModifier (-n)]

instance HasAbilities ForbiddenTomeSecretsRevealed3 where
  getAbilities (ForbiddenTomeSecretsRevealed3 a) =
    [ controlledAbility
        a
        1
        (exists $ oneOf [AccessibleLocation, YourLocation <> LocationWithAnyClues])
        $ ActionAbility [] (ActionCost 4 <> exhaust a)
    ]

instance RunMessage ForbiddenTomeSecretsRevealed3 where
  runMessage msg a@(ForbiddenTomeSecretsRevealed3 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      lids <- getAccessibleLocations iid attrs
      player <- getPlayer iid
      pushAll
        [ chooseOrRunOne player
            $ Label "Do not move" []
            : [targetLabel lid [Move $ move (toSource attrs) iid lid] | lid <- lids]
        , Msg.DiscoverClues iid $ discoverAtYourLocation (toAbilitySource attrs 1) 1
        ]
      pure a
    _ -> ForbiddenTomeSecretsRevealed3 <$> runMessage msg attrs
