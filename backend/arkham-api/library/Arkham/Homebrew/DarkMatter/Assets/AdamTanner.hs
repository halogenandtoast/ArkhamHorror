module Arkham.Homebrew.DarkMatter.Assets.AdamTanner (adamTanner) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Matcher
import Arkham.Window (windowType)
import Arkham.Window qualified as Window

newtype AdamTanner = AdamTanner AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

adamTanner :: AssetCard AdamTanner
adamTanner = ally AdamTanner Cards.adamTanner (2, 1)

{- | "[reaction] When an enemy spawns at your location: You automatically evade it.
(Group limit once per game.)"
-}
instance HasAbilities AdamTanner where
  getAbilities (AdamTanner a) =
    [ groupLimit PerGame
        $ controlled a 1 ControlsThis
        $ freeReaction
        $ EnemySpawns #when (locationWithAsset a.id) AnyEnemy
    ]

instance RunMessage AdamTanner where
  runMessage msg a@(AdamTanner attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      putCardIntoPlay iid attrs
      pure a
    -- "You automatically evade *it*" — the enemy that spawned, which at #when is
    -- not yet placed, so it can only come from the window.
    UseCardAbility iid (isSource attrs -> True) 1 (map windowType -> [Window.EnemySpawns eid _]) _ -> do
      automaticallyEvadeEnemy iid eid
      pure a
    _ -> AdamTanner <$> liftRunMessage msg attrs
