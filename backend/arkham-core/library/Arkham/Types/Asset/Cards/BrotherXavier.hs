{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.BrotherXavier
  ( brotherXavier
  , BrotherXavier(..)
  )
where

import Arkham.Import

import Arkham.Types.Asset.Attrs

newtype BrotherXavier = BrotherXavier Attrs
  deriving newtype (Show, ToJSON, FromJSON)

brotherXavier :: AssetId -> BrotherXavier
brotherXavier uuid =
  BrotherXavier $ (baseAttrs uuid "02106")
  { assetSlots = [AllySlot]
  , assetHealth = Just 3
  , assetSanity = Just 3
  }

instance HasModifiersFor env BrotherXavier where
  getModifiersFor _ (InvestigatorTarget iid) (BrotherXavier a) =
    pure [ SkillModifier SkillWillpower 1 | ownedBy a iid ]
  getModifiersFor _ _ _ = pure []

instance (HasQueue env, HasModifiers env InvestigatorId) => RunMessage env BrotherXavier where
  runMessage msg (BrotherXavier attrs) = BrotherXavier <$> runMessage msg attrs

ability :: Attrs -> Ability
ability attrs =
  mkAbility (toSource attrs) 1 (ReactionAbility (AssetDefeated iid))

instance ActionRunner env => HasActions env BrotherXavier where
  getActions iid (WhenDefeated source) (BrotherXavier a) | ownedBy a iid = do
    isSource a source = do
      [ uncurry ActivateCardAbilityAction iid (ability a) ]
  getActions _ _ _ = pure []

instance AssetRunner env => RunMessage env BrotherXavier where
  runMessage msg a@(BrotherXavier attrs) = case msg of
    AssetDefeated iid | getInvestigator attrs == iid ->
      a <$ unshiftMessage
        (chooseOne
          iid
          [ UseCardAbility iid (toSource attrs) Nothing 1
          , Continue "Do not use Brother Xavier's ability"
          ]
        )
    UseCardAbility iid source _ 1 | isSource attrs source ->
      locationId <- getId @LocationId (getInvestigator attrs)
      locationEnemyIds <- getSetList locationId
      unshiftMessages
        [chooseOne
          iid
          [ EnemyDamage eid iid (toSource attrs) 2 | eid <- locationEnemyIds ]
        ]
    _ -> BrotherXavier <$> runMessage msg attrs
