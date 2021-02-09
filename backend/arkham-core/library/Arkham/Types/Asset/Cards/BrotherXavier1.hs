module Arkham.Types.Asset.Cards.BrotherXavier1
  ( brotherXavier1
  , BrotherXavier1(..)
  )
where


import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner

newtype BrotherXavier1 = BrotherXavier1 AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

brotherXavier1 :: AssetId -> BrotherXavier1
brotherXavier1 uuid = BrotherXavier1 $ (baseAttrs uuid "02106")
  { assetSlots = [AllySlot]
  , assetHealth = Just 3
  , assetSanity = Just 3
  }

instance (HasId LocationId env InvestigatorId) => HasModifiersFor env BrotherXavier1 where
  getModifiersFor _ (InvestigatorTarget iid) (BrotherXavier1 a)
    | ownedBy a iid = pure $ toModifiers a [SkillModifier SkillWillpower 1]
  getModifiersFor (InvestigatorSource iid) target (BrotherXavier1 a)
    | isTarget a target = do
      locationId <- getId @LocationId iid
      assetLocationId <- getId @LocationId
        $ fromJustNote "unowned" (assetInvestigator a)
      pure
        [ toModifier a CanBeAssignedDamage
        | locationId == assetLocationId && Just iid /= assetInvestigator a
        ]
  getModifiersFor _ _ _ = pure []

ability :: AssetAttrs -> Ability
ability attrs = mkAbility (toSource attrs) 1 (ReactionAbility Free)

instance HasActions env BrotherXavier1 where
  getActions iid (WhenDefeated source) (BrotherXavier1 a) | isSource a source =
    pure [ ActivateCardAbilityAction iid (ability a) | ownedBy a iid ]
  getActions _ _ _ = pure []

instance AssetRunner env => RunMessage env BrotherXavier1 where
  runMessage msg a@(BrotherXavier1 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      locationId <- getId @LocationId (getInvestigator attrs)
      locationEnemyIds <- getSetList locationId
      a <$ unshiftMessages
        [ chooseOne
            iid
            [ EnemyDamage eid iid (toSource attrs) 2 | eid <- locationEnemyIds ]
        ]
    _ -> BrotherXavier1 <$> runMessage msg attrs
