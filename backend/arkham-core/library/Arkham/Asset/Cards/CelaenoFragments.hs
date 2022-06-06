module Arkham.Asset.Cards.CelaenoFragments where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Id
import Arkham.Modifier
import Arkham.Query
import Arkham.SkillType
import Arkham.Target

newtype CelaenoFragments = CelaenoFragments AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

celaenoFragments :: AssetCard CelaenoFragments
celaenoFragments = asset CelaenoFragments Cards.celaenoFragments

instance HasCount CardCount env InvestigatorId => HasModifiersFor CelaenoFragments where
  getModifiersFor _ (InvestigatorTarget iid) (CelaenoFragments attrs)
    | controlledBy attrs iid = do
      count' <- unCardCount <$> getCount iid
      pure
        . toModifiers attrs
        $ [ SkillModifier SkillIntellect 1 | count' >= 5 ]
        <> [ SkillModifier SkillWillpower 1 | count' >= 10 ]
        <> [ SkillModifier SkillIntellect 1 | count' >= 15 ]
  getModifiersFor _ _ _ = pure []

instance (AssetRunner env) => RunMessage CelaenoFragments where
  runMessage msg (CelaenoFragments attrs) =
    CelaenoFragments <$> runMessage msg attrs
