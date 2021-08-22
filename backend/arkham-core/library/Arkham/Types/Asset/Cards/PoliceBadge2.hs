module Arkham.Types.Asset.Cards.PoliceBadge2
  ( PoliceBadge2(..)
  , policeBadge2
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype PoliceBadge2 = PoliceBadge2 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

policeBadge2 :: AssetCard PoliceBadge2
policeBadge2 = accessory PoliceBadge2 Cards.policeBadge2

instance HasModifiersFor env PoliceBadge2 where
  getModifiersFor _ (InvestigatorTarget iid) (PoliceBadge2 a) =
    pure [ toModifier a (SkillModifier SkillWillpower 1) | ownedBy a iid ]
  getModifiersFor _ _ _ = pure []

instance HasAbilities env PoliceBadge2 where
  getAbilities _ _ (PoliceBadge2 a) = pure
    [restrictedAbility a 1 criteria $ FastAbility $ DiscardCost (toTarget a)]
   where
    criteria = OwnsThis
      <> InvestigatorExists (TurnInvestigator <> InvestigatorAt YourLocation)

instance AssetRunner env => RunMessage env PoliceBadge2 where
  runMessage msg a@(PoliceBadge2 attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      selectOne TurnInvestigator >>= \case
        Nothing -> error "must exist"
        Just iid -> a <$ push (GainActions iid source 2)
    _ -> PoliceBadge2 <$> runMessage msg attrs
