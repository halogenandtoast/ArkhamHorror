module Arkham.Asset.Cards.TomeOfRituals (
  tomeOfRituals,
  TomeOfRituals (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher

newtype TomeOfRituals = TomeOfRituals AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

tomeOfRituals :: AssetCard TomeOfRituals
tomeOfRituals = asset TomeOfRituals Cards.tomeOfRituals

instance HasAbilities TomeOfRituals where
  getAbilities (TomeOfRituals a) =
    [ restrictedAbility a 1 (ControlsThis <> exists (NotInvestigator You))
        $ ForcedAbility
        $ Matcher.InvestigatorDefeated #when ByAny You
    ]

instance RunMessage TomeOfRituals where
  runMessage msg a@(TomeOfRituals attrs) = case msg of
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
    _ -> TomeOfRituals <$> runMessage msg attrs
