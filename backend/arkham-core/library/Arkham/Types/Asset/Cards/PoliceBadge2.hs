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
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.Restriction
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

instance HasActions PoliceBadge2 where
  getActions (PoliceBadge2 a) =
    [ restrictedAbility
          a
          1
          (OwnsThis <> InvestigatorExists
            (TurnInvestigator <> InvestigatorAt YourLocation)
          )
        $ FastAbility
        $ DiscardCost (toTarget a)
    ]

instance AssetRunner env => RunMessage env PoliceBadge2 where
  runMessage msg a@(PoliceBadge2 attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      activeInvestigatorId <- unActiveInvestigatorId <$> getId ()
      a <$ push (GainActions activeInvestigatorId source 2)
    _ -> PoliceBadge2 <$> runMessage msg attrs
