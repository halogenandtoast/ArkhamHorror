module Arkham.Types.Asset.Cards.PeterSylvestre2
  ( PeterSylvestre2(..)
  , peterSylvestre2
  )
where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Window

newtype PeterSylvestre2 = PeterSylvestre2 AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

peterSylvestre2 :: AssetCard PeterSylvestre2
peterSylvestre2 = ally PeterSylvestre2 Cards.peterSylvestre2 (1, 3)

instance HasModifiersFor env PeterSylvestre2 where
  getModifiersFor _ (InvestigatorTarget iid) (PeterSylvestre2 a)
    | ownedBy a iid = pure $ toModifiers
      a
      [SkillModifier SkillAgility 1, SkillModifier SkillWillpower 1]
  getModifiersFor _ _ _ = pure []

ability :: AssetAttrs -> Ability
ability attrs = mkAbility (toSource attrs) 1 (ReactionAbility Free)

instance HasActions env PeterSylvestre2 where
  getActions iid (AfterEndTurn You) (PeterSylvestre2 a) | ownedBy a iid =
    pure [ ActivateCardAbilityAction iid (ability a) | assetSanityDamage a > 0 ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env PeterSylvestre2 where
  runMessage msg (PeterSylvestre2 attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      pure $ PeterSylvestre2 $ attrs & sanityDamageL -~ 1
    _ -> PeterSylvestre2 <$> runMessage msg attrs
