module Arkham.Asset.Cards.TimeWornLocket (
  timeWornLocket,
  TimeWornLocket (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher

newtype TimeWornLocket = TimeWornLocket AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

timeWornLocket :: AssetCard TimeWornLocket
timeWornLocket = asset TimeWornLocket Cards.timeWornLocket

instance HasAbilities TimeWornLocket where
  getAbilities (TimeWornLocket a) =
    [ restrictedAbility a 1 (ControlsThis <> exists (NotInvestigator You))
        $ ForcedAbility
        $ Matcher.InvestigatorDefeated #when ByAny You
    ]

instance RunMessage TimeWornLocket where
  runMessage msg a@(TimeWornLocket attrs) = case msg of
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
    _ -> TimeWornLocket <$> runMessage msg attrs
