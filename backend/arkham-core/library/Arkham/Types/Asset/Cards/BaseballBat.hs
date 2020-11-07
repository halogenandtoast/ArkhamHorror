{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.BaseballBat
  ( BaseballBat(..)
  , baseballBat
  )
where

import Arkham.Import

import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import qualified Arkham.Types.Token as Token
import Arkham.Types.TokenResponse

newtype BaseballBat = BaseballBat Attrs
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

baseballBat :: AssetId -> BaseballBat
baseballBat uuid =
  BaseballBat $ baseAttrs uuid "01074" $ slots .= [HandSlot, HandSlot]

instance HasModifiersFor env BaseballBat where
  getModifiersFor _ _ _ = pure []

fightAbility :: Attrs -> Ability
fightAbility Attrs { assetId } =
  mkAbility (AssetSource assetId) 1 (ActionAbility 1 (Just Action.Fight))

instance ActionRunner env  => HasActions env BaseballBat where
  getActions iid window (BaseballBat a@Attrs {..}) | ownedBy a iid = do
    fightAvailable <- hasFightActions iid window
    pure [ ActivateCardAbilityAction iid (fightAbility a) | fightAvailable ]
  getActions _ _ _ = pure []


instance (AssetRunner env) => RunMessage env BaseballBat where
  runMessage msg a@(BaseballBat attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source -> a <$ unshiftMessage
      (ChooseFightEnemy
        iid
        source
        SkillCombat
        [SkillModifier SkillCombat 2]
        [ OnAnyToken
            [Token.Skull, Token.AutoFail]
            [Discard (AssetTarget $ assetId attrs)]
        ]
        False
      )
    _ -> BaseballBat <$> runMessage msg attrs
