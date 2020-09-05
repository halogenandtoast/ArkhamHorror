{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.Pickpocketing where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Window
import ClassyPrelude
import Lens.Micro

newtype Pickpocketing = Pickpocketing Attrs
  deriving newtype (Show, ToJSON, FromJSON)

pickpoketing :: AssetId -> Pickpocketing
pickpoketing uuid = Pickpocketing $ baseAttrs uuid "01046"

instance (ActionRunner env investigator) => HasActions env investigator Pickpocketing where
  getActions i window@(WhenEnemyEvaded You) (Pickpocketing attrs@Attrs {..})
    | getId () i `elem` assetInvestigator = do
      baseActions <- getActions i window attrs
      let ability = mkAbility (AssetSource assetId) 1 (ReactionAbility window)
      pure
        $ baseActions
        <> [ ActivateCardAbilityAction (getId () i) ability
           | not assetExhausted
           ]
  getActions i window (Pickpocketing attrs) = getActions i window attrs

instance (AssetRunner env) => RunMessage env Pickpocketing where
  runMessage msg (Pickpocketing attrs@Attrs {..}) = case msg of
    UseCardAbility iid _ (AssetSource aid) _ 1 | aid == assetId -> do
      unshiftMessage (DrawCards iid 1 False)
      pure $ Pickpocketing $ attrs & exhausted .~ True
    _ -> Pickpocketing <$> runMessage msg attrs
