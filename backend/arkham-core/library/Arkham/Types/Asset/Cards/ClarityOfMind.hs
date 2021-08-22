module Arkham.Types.Asset.Cards.ClarityOfMind
  ( clarityOfMind
  , ClarityOfMind(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target

newtype ClarityOfMind = ClarityOfMind AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

clarityOfMind :: AssetCard ClarityOfMind
clarityOfMind = arcane ClarityOfMind Cards.clarityOfMind

instance HasAbilities env ClarityOfMind where
  getAbilities _ _ (ClarityOfMind a) = pure
    [ restrictedAbility
        a
        1
        (OwnsThis <> InvestigatorExists
          (InvestigatorAt YourLocation <> InvestigatorWithAnyHorror)
        )
      $ ActionAbility Nothing
      $ Costs [ActionCost 1, UseCost (toId a) Charge 1]
    ]

instance AssetRunner env => RunMessage env ClarityOfMind where
  runMessage msg a@(ClarityOfMind attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      targets <- selectListMap InvestigatorTarget (InvestigatorAt YourLocation)
      a <$ push
        (chooseOrRunOne
          iid
          [ TargetLabel target [HealHorror target 1] | target <- targets ]
        )
    _ -> ClarityOfMind <$> runMessage msg attrs
