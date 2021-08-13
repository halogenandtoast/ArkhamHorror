module Arkham.Types.Asset.Cards.PeterSylvestre
  ( PeterSylvestre(..)
  , peterSylvestre
  ) where

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
import Arkham.Types.Restriction
import Arkham.Types.SkillType
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing

newtype PeterSylvestre = PeterSylvestre AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

peterSylvestre :: AssetCard PeterSylvestre
peterSylvestre = ally PeterSylvestre Cards.peterSylvestre (1, 2)

instance HasModifiersFor env PeterSylvestre where
  getModifiersFor _ (InvestigatorTarget iid) (PeterSylvestre a) =
    pure [ toModifier a (SkillModifier SkillAgility 1) | ownedBy a iid ]
  getModifiersFor _ _ _ = pure []

instance HasActions PeterSylvestre where
  getActions (PeterSylvestre x) =
    [ restrictedAbility
        x
        1
        (OwnsThis <> AssetExists (AssetWithId (toId x) <> AssetWithHorror))
        (ReactionAbility (TurnEnds Timing.After You) Free)
    ]

instance AssetRunner env => RunMessage env PeterSylvestre where
  runMessage msg (PeterSylvestre attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      pure $ PeterSylvestre $ attrs & sanityDamageL -~ 1
    _ -> PeterSylvestre <$> runMessage msg attrs
