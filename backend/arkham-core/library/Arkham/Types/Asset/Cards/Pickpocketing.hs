{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.Pickpocketing where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner

newtype Pickpocketing = Pickpocketing Attrs
  deriving newtype (Show, ToJSON, FromJSON)

pickpoketing :: AssetId -> Pickpocketing
pickpoketing uuid = Pickpocketing $ baseAttrs uuid "01046"

instance HasModifiersFor env investigator Pickpocketing where
  getModifiersFor _ _ _ = pure []

instance (ActionRunner env investigator) => HasActions env investigator Pickpocketing where
  getActions i window@(WhenEnemyEvaded You) (Pickpocketing a) | ownedBy a i = do
    baseActions <- getActions i window a
    let ability = mkAbility (toSource a) 1 (ReactionAbility window)
    pure
      $ baseActions
      <> [ ActivateCardAbilityAction (getId () i) ability
         | not (assetExhausted a)
         ]
  getActions i window (Pickpocketing a) = getActions i window a

instance (AssetRunner env) => RunMessage env Pickpocketing where
  runMessage msg (Pickpocketing attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      unshiftMessage (DrawCards iid 1 False)
      pure $ Pickpocketing $ attrs & exhausted .~ True
    _ -> Pickpocketing <$> runMessage msg attrs
