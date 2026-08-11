module Arkham.Homebrew.DarkMatter.Assets.AdamTanner (adamTanner) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Matcher

newtype AdamTanner = AdamTanner AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

adamTanner :: AssetCard AdamTanner
adamTanner = asset AdamTanner Cards.adamTanner

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
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select $ EnemyAt (locationWithAsset attrs.id)
      for_ enemies $ automaticallyEvadeEnemy iid
      pure a
    _ -> AdamTanner <$> liftRunMessage msg attrs
