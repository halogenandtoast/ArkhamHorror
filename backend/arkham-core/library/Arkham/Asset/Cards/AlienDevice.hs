module Arkham.Asset.Cards.AlienDevice (
  alienDevice,
  AlienDevice (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher

newtype AlienDevice = AlienDevice AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

alienDevice :: AssetCard AlienDevice
alienDevice = asset AlienDevice Cards.alienDevice

instance HasAbilities AlienDevice where
  getAbilities (AlienDevice a) =
    [ restrictedAbility a 1 (ControlsThis <> exists (NotInvestigator You))
        $ ForcedAbility
        $ Matcher.InvestigatorDefeated #when ByAny You
    ]

instance RunMessage AlienDevice where
  runMessage msg a@(AlienDevice attrs) = case msg of
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
    _ -> AlienDevice <$> runMessage msg attrs
