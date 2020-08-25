{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.BaseballBat where

import Arkham.Json
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Target
import qualified Arkham.Types.Token as Token
import Arkham.Types.TokenResponse
import ClassyPrelude

newtype BaseballBat = BaseballBat Attrs
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

baseballBat :: AssetId -> BaseballBat
baseballBat uuid =
  BaseballBat $ (baseAttrs uuid "01074") { assetSlots = [HandSlot, HandSlot] }

instance (ActionRunner env investigator) => HasActions env investigator BaseballBat where
  getActions i window (BaseballBat Attrs {..})
    | Just (getId () i) == assetInvestigator = do
      fightAvailable <- hasFightActions i window
      pure
        [ ActivateCardAbilityAction
            (getId () i)
            (mkAbility
              (AssetSource assetId)
              1
              (ActionAbility 1 (Just Action.Fight))
            )
        | fightAvailable
        ]
  getActions _ _ _ = pure []


instance (AssetRunner env) => RunMessage env BaseballBat where
  runMessage msg a@(BaseballBat attrs@Attrs {..}) = case msg of
    UseCardAbility iid _ (AssetSource aid) 1 | aid == assetId ->
      a <$ unshiftMessage
        (ChooseFightEnemy
          iid
          SkillCombat
          [SkillModifier SkillCombat 2 (AssetSource aid)]
          [ OnAnyToken
              [Token.Skull, Token.AutoFail]
              [Discard (AssetTarget assetId)]
          ]
          False
        )
    _ -> BaseballBat <$> runMessage msg attrs
