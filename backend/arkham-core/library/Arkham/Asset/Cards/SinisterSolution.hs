module Arkham.Asset.Cards.SinisterSolution (
  sinisterSolution,
  SinisterSolution (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher

newtype SinisterSolution = SinisterSolution AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

sinisterSolution :: AssetCard SinisterSolution
sinisterSolution = asset SinisterSolution Cards.sinisterSolution

instance HasAbilities SinisterSolution where
  getAbilities (SinisterSolution a) =
    [ restrictedAbility a 1 (ControlsThis <> exists (NotInvestigator You))
        $ ForcedAbility
        $ Matcher.InvestigatorDefeated #when ByAny You
    ]

instance RunMessage SinisterSolution where
  runMessage msg a@(SinisterSolution attrs) = case msg of
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
    _ -> SinisterSolution <$> runMessage msg attrs
