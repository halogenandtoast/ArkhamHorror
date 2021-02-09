module Arkham.Types.Asset.Cards.DiscOfItzamna2 where


import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Trait

newtype DiscOfItzamna2 = DiscOfItzamna2 AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

discOfItzamna2 :: AssetId -> DiscOfItzamna2
discOfItzamna2 uuid =
  DiscOfItzamna2 $ (baseAttrs uuid "01059") { assetSlots = [AccessorySlot] }

instance HasModifiersFor env DiscOfItzamna2 where
  getModifiersFor = noModifiersFor

instance HasActions env DiscOfItzamna2 where
  getActions iid (WhenEnemySpawns YourLocation traits) (DiscOfItzamna2 a)
    | ownedBy a iid = pure
      [ ActivateCardAbilityAction
          iid
          (mkAbility (toSource a) 1 (ReactionAbility Free))
      | Elite `notElem` traits
      ]
  getActions i window (DiscOfItzamna2 x) = getActions i window x

instance (AssetRunner env) => RunMessage env DiscOfItzamna2 where
  runMessage msg a@(DiscOfItzamna2 attrs) = case msg of
    InvestigatorPlayAsset iid aid _ _ | aid == assetId attrs -> do
      unshiftMessage
        (CreateWindowModifierEffect
          EffectSkillTestWindow
          (EffectModifiers [toModifier attrs $ SkillModifier SkillWillpower 1])
          (toSource attrs)
          (InvestigatorTarget iid)
        )
      DiscOfItzamna2 <$> runMessage msg attrs
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      menemySpawnMessage <- fromQueue
        $ find ((== Just EnemySpawnMessage) . messageType)
      a <$ case menemySpawnMessage of
        Just (EnemySpawn _ _ eid) ->
          unshiftMessages [Discard (toTarget attrs), Discard (EnemyTarget eid)]
        _ -> pure ()
    _ -> DiscOfItzamna2 <$> runMessage msg attrs
