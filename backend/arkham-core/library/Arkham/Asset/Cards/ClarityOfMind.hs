module Arkham.Asset.Cards.ClarityOfMind
  ( clarityOfMind
  , ClarityOfMind(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Helpers.Investigator
import Arkham.Matcher

newtype ClarityOfMind = ClarityOfMind AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

clarityOfMind :: AssetCard ClarityOfMind
clarityOfMind = asset ClarityOfMind Cards.clarityOfMind

instance HasAbilities ClarityOfMind where
  getAbilities (ClarityOfMind a) =
    [ restrictedAbility
          a
          1
          (ControlsThis <> InvestigatorExists
            (InvestigatorAt YourLocation <> InvestigatorWithAnyHorror)
          )
        $ ActionAbility Nothing
        $ Costs [ActionCost 1, UseCost (AssetWithId $ toId a) Charge 1]
    ]

instance RunMessage ClarityOfMind where
  runMessage msg a@(ClarityOfMind attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      iids <- selectList $ colocatedWith iid
      iidsWithHeal <- mapMaybeM
        (\iid' -> (iid', ) <$$> getHealHorrorMessage attrs 1 iid')
        iids
      push $ chooseOrRunOne iid $ map (uncurry targetLabel . second pure) iidsWithHeal
      pure a
    _ -> ClarityOfMind <$> runMessage msg attrs
