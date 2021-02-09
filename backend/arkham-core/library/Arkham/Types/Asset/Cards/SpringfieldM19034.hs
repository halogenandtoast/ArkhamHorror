module Arkham.Types.Asset.Cards.SpringfieldM19034
  ( springfieldM19034
  , SpringfieldM19034(..)
  )
where


import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Uses (Uses(..), useCount)
import qualified Arkham.Types.Asset.Uses as Resource

newtype SpringfieldM19034 = SpringfieldM19034 AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

springfieldM19034 :: AssetId -> SpringfieldM19034
springfieldM19034 uuid = SpringfieldM19034
  $ (baseAttrs uuid "02226") { assetSlots = [HandSlot, HandSlot] }

instance ActionRunner env => HasActions env SpringfieldM19034 where
  getActions iid window (SpringfieldM19034 a) | ownedBy a iid = do
    fightAvailable <- hasFightActions iid window
    pure
      [ ActivateCardAbilityAction
          iid
          (mkAbility
            (toSource a)
            1
            (ActionAbility (Just Action.Fight) (ActionCost 1))
          )
      | useCount (assetUses a) > 0 && fightAvailable
      ]
  getActions _ _ _ = pure []

instance HasModifiersFor env SpringfieldM19034 where
  getModifiersFor = noModifiersFor

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env SpringfieldM19034 where
  runMessage msg (SpringfieldM19034 attrs) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId attrs ->
      SpringfieldM19034
        <$> runMessage msg (attrs & usesL .~ Uses Resource.Ammo 3)
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      unshiftMessages
        [ CreateWindowModifierEffect EffectSkillTestWindow
          (EffectModifiers
          $ toModifiers attrs [DamageDealt 2, SkillModifier SkillCombat 3]
          )
          source
          (InvestigatorTarget iid)
        , ChooseFightEnemyNotEngagedWithInvestigator
          iid
          source
          SkillCombat
          False
        ]
      pure $ SpringfieldM19034 $ attrs & usesL %~ Resource.use
    _ -> SpringfieldM19034 <$> runMessage msg attrs
