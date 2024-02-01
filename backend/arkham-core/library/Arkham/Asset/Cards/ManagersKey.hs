module Arkham.Asset.Cards.ManagersKey (
  managersKey,
  ManagersKey (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher

newtype ManagersKey = ManagersKey AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

managersKey :: AssetCard ManagersKey
managersKey = asset ManagersKey Cards.managersKey

instance HasAbilities ManagersKey where
  getAbilities (ManagersKey a) =
    [ restrictedAbility a 1 (ControlsThis <> exists (NotInvestigator You))
        $ ForcedAbility
        $ Matcher.InvestigatorDefeated #when ByAny You
    ]

instance RunMessage ManagersKey where
  runMessage msg a@(ManagersKey attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ putCardIntoPlay iid attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      player <- getPlayer iid
      investigators <- selectList $ NotInvestigator $ InvestigatorWithId iid
      push
        $ chooseOrRunOne player
        $ targetLabels investigators
        $ only
        . (`TakeControlOfAsset` toId attrs)
      pure a
    _ -> ManagersKey <$> runMessage msg attrs
