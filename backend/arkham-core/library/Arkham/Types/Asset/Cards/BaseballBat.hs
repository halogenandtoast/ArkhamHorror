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

newtype BaseballBat = BaseballBat Attrs
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

baseballBat :: AssetId -> BaseballBat
baseballBat uuid =
  BaseballBat $ (baseAttrs uuid "01074") { assetSlots = [HandSlot, HandSlot] }

instance HasModifiersFor env BaseballBat where
  getModifiersFor = noModifiersFor

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
    UseCardAbility iid source _ 1 | isSource attrs source ->
      a <$ unshiftMessages
        [ CreateSkillTestEffect
          (EffectModifiers [SkillModifier SkillCombat 2])
          source
          (InvestigatorTarget iid)
        , CreateEffect "01074" Nothing source (InvestigatorTarget iid)
        , ChooseFightEnemy iid source SkillCombat False
        ]
    _ -> BaseballBat <$> runMessage msg attrs
