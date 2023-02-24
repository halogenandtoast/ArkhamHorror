module Arkham.Asset.Cards.CelaenoFragments where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Investigator.Types ( Field(..) )
import Arkham.Projection
import Arkham.SkillType

newtype CelaenoFragments = CelaenoFragments AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

celaenoFragments :: AssetCard CelaenoFragments
celaenoFragments = asset CelaenoFragments Cards.celaenoFragments

instance HasModifiersFor CelaenoFragments where
  getModifiersFor (InvestigatorTarget iid) (CelaenoFragments attrs)
    | controlledBy attrs iid = do
      count' <- fieldMap InvestigatorHand length iid
      pure
        . toModifiers attrs
        $ [ SkillModifier SkillIntellect 1 | count' >= 5 ]
        <> [ SkillModifier SkillWillpower 1 | count' >= 10 ]
        <> [ SkillModifier SkillIntellect 1 | count' >= 15 ]
  getModifiersFor _ _ = pure []

instance RunMessage CelaenoFragments where
  runMessage msg (CelaenoFragments attrs) =
    CelaenoFragments <$> runMessage msg attrs
