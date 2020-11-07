{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.CelaenoFragments where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner

newtype CelaenoFragments = CelaenoFragments Attrs
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

celaenoFragments :: AssetId -> CelaenoFragments
celaenoFragments uuid =
  CelaenoFragments $ baseAttrs uuid "60206" $ slots .= [HandSlot]

instance AssetRunner env => HasModifiersFor env CelaenoFragments where
  getModifiersFor _ (InvestigatorTarget iid) (CelaenoFragments attrs)
    | ownedBy attrs iid = do
      count' <- asks $ unCardCount . getCount iid
      pure
        $ [ SkillModifier SkillIntellect 1 | count' >= 5 ]
        <> [ SkillModifier SkillWillpower 1 | count' >= 10 ]
        <> [ SkillModifier SkillIntellect 1 | count' >= 15 ]
  getModifiersFor _ _ _ = pure []

instance HasActions env CelaenoFragments where
  getActions i window (CelaenoFragments x) = getActions i window x

instance (AssetRunner env) => RunMessage env CelaenoFragments where
  runMessage msg (CelaenoFragments attrs) =
    CelaenoFragments <$> runMessage msg attrs
