{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.Shrivelling where

import Arkham.Json
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses (Uses(..))
import qualified Arkham.Types.Asset.Uses as Resource
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Helpers
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import qualified Arkham.Types.Token as Token
import ClassyPrelude
import Lens.Micro

newtype ShrivellingMetadata = ShrivellingMetadata { inUse :: Bool }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON) -- must parse to object

newtype Shrivelling = Shrivelling (Attrs `With` ShrivellingMetadata)
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

shrivelling :: AssetId -> Shrivelling
shrivelling uuid =
  Shrivelling
    $ ((baseAttrs uuid "01060") { assetSlots = [ArcaneSlot] })
    `with` ShrivellingMetadata False

instance (AssetRunner env) => RunMessage env Shrivelling where
  runMessage msg a@(Shrivelling (attrs@Attrs {..} `With` metadata@ShrivellingMetadata {..}))
    = case msg of
      InvestigatorPlayAsset _ aid _ _ | aid == assetId -> do
        let
          attrs' =
            attrs
              & (uses .~ Uses Resource.Charge 4)
              & (abilities
                .~ [ ( AssetSource aid
                     , Nothing
                     , 1
                     , ActionAbility 1 (Just Action.Fight)
                     , NoLimit
                     )
                   ]
                )
        Shrivelling . (`with` metadata) <$> runMessage msg attrs'
      SkillTestEnded _ tokens | inUse -> do
        when
            (any
              (`elem` [ Token.Skull
                      , Token.Cultist
                      , Token.Tablet
                      , Token.ElderThing
                      , Token.AutoFail
                      ]
              )
              tokens
            )
          $ unshiftMessage
              (InvestigatorDamage
                (getInvestigator attrs)
                (AssetSource assetId)
                0
                1
              )
        pure $ Shrivelling (attrs `with` ShrivellingMetadata False)
      UseCardAbility iid (AssetSource aid, _, 1, _, _) | aid == assetId ->
        case assetUses of
          Uses Resource.Charge n -> do
            when
              (n == 1)
              (unshiftMessage (RemoveAbilitiesFrom (AssetSource assetId)))
            unshiftMessage
              (ChooseFightEnemy
                iid
                SkillWillpower
                [DamageDealt 1 (AssetSource aid)]
                mempty -- TODO: Add metadata response here
                False
              )
            pure
              $ Shrivelling
              . (`with` ShrivellingMetadata True)
              $ attrs
              & uses
              .~ Uses Resource.Charge (n - 1)
          _ -> pure a
      _ -> Shrivelling . (`with` metadata) <$> runMessage msg attrs
