module Arkham.Asset.Cards.TheSilverKey (theSilverKey, TheSilverKey (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype TheSilverKey = TheSilverKey AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSilverKey :: AssetCard TheSilverKey
theSilverKey = asset TheSilverKey Cards.theSilverKey

instance HasAbilities TheSilverKey where
  getAbilities (TheSilverKey a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility (InvestigatorWouldTakeHorror #when You (SourceIsCancelable AnySource)) (exhaust a)
    ]

instance RunMessage TheSilverKey where
  runMessage msg a@(TheSilverKey attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      cancelHorror iid (attrs.ability 1) 1 []
      pure a
    _ -> TheSilverKey <$> runMessage msg attrs
