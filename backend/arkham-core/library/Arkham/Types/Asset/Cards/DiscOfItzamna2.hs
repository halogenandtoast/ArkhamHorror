module Arkham.Types.Asset.Cards.DiscOfItzamna2 where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.Window

newtype DiscOfItzamna2 = DiscOfItzamna2 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

discOfItzamna2 :: AssetCard DiscOfItzamna2
discOfItzamna2 = accessory DiscOfItzamna2 Cards.discOfItzamna2

instance HasModifiersFor env DiscOfItzamna2

instance HasSet Trait env EnemyId => HasAbilities env DiscOfItzamna2 where
  getAbilities iid (WhenEnemySpawns eid _) (DiscOfItzamna2 a) | ownedBy a iid = do
    traits <- getSet eid
    pure
      [ mkAbility (toSource a) 1 (ResponseAbility Free)
      | Elite `notElem` traits
      ]
  getAbilities i window (DiscOfItzamna2 x) = getAbilities i window x

instance (AssetRunner env) => RunMessage env DiscOfItzamna2 where
  runMessage msg a@(DiscOfItzamna2 attrs) = case msg of
    InvestigatorPlayAsset iid aid _ _ | aid == assetId attrs -> do
      push
        (skillTestModifier
          attrs
          (InvestigatorTarget iid)
          (SkillModifier SkillWillpower 1)
        )
      DiscOfItzamna2 <$> runMessage msg attrs
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      menemySpawnMessage <- fromQueue
        $ find ((== Just EnemySpawnMessage) . messageType)
      a <$ case menemySpawnMessage of
        Just msg'@(EnemySpawn _ _ eid) -> replaceMessage
          msg'
          [Discard (toTarget attrs), Discard (EnemyTarget eid)]
        _ -> pure ()
    _ -> DiscOfItzamna2 <$> runMessage msg attrs
