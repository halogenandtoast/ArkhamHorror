{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.Shrivelling where

import Arkham.Import
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses (Uses(..), useCount)
import qualified Arkham.Types.Asset.Uses as Resource
import qualified Arkham.Types.Token as Token
import Arkham.Types.TokenResponse

newtype Shrivelling = Shrivelling Attrs
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

shrivelling :: AssetId -> Shrivelling
shrivelling uuid = Shrivelling $ baseAttrs uuid "01060" $ slots .= [ArcaneSlot]

instance HasModifiersFor env Shrivelling where
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env Shrivelling where
  getActions iid window (Shrivelling a) | ownedBy a iid = do
    fightAvailable <- hasFightActions iid window
    pure
      [ ActivateCardAbilityAction
          iid
          (mkAbility (toSource a) 1 (ActionAbility 1 (Just Action.Fight)))
      | useCount (assetUses a) > 0 && fightAvailable
      ]
  getActions _ _ _ = pure []

instance AssetRunner env => RunMessage env Shrivelling where
  runMessage msg (Shrivelling attrs) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId attrs ->
      Shrivelling <$> runMessage msg (attrs & uses .~ Uses Resource.Charge 4)
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      unshiftMessage $ ChooseFightEnemy
        iid
        source
        SkillWillpower
        [DamageDealt 1]
        [ OnAnyToken
            [ Token.Skull
            , Token.Cultist
            , Token.Tablet
            , Token.ElderThing
            , Token.AutoFail
            ]
            [InvestigatorAssignDamage (getInvestigator attrs) source 0 1]
        ]
        False
      pure $ Shrivelling $ attrs & uses %~ Resource.use
    _ -> Shrivelling <$> runMessage msg attrs
