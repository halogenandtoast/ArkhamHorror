module Arkham.Types.Asset.Cards.CelaenoFragments where

import Arkham.Prelude

import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Target
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner

newtype CelaenoFragments = CelaenoFragments AssetAttrs
  deriving newtype (Show, Eq, Generic, ToJSON, FromJSON, Entity)

celaenoFragments :: AssetId -> CelaenoFragments
celaenoFragments uuid =
  CelaenoFragments $ (baseAttrs uuid "60206") { assetSlots = [HandSlot] }

instance HasCount CardCount env InvestigatorId => HasModifiersFor env CelaenoFragments where
  getModifiersFor _ (InvestigatorTarget iid) (CelaenoFragments attrs)
    | ownedBy attrs iid = do
      count' <- unCardCount <$> getCount iid
      pure
        . toModifiers attrs
        $ [ SkillModifier SkillIntellect 1 | count' >= 5 ]
        <> [ SkillModifier SkillWillpower 1 | count' >= 10 ]
        <> [ SkillModifier SkillIntellect 1 | count' >= 15 ]
  getModifiersFor _ _ _ = pure []

instance HasActions env CelaenoFragments where
  getActions i window (CelaenoFragments x) = getActions i window x

instance (AssetRunner env) => RunMessage env CelaenoFragments where
  runMessage msg (CelaenoFragments attrs) =
    CelaenoFragments <$> runMessage msg attrs
