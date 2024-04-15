module Arkham.Asset.Cards.TheCaptain (theCaptain, TheCaptain (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding (chooseOne)
import Arkham.Matcher
import Arkham.Message.Lifted
import Arkham.Prelude

newtype TheCaptain = TheCaptain AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCaptain :: AssetCard TheCaptain
theCaptain = asset TheCaptain Cards.theCaptain

instance HasAbilities TheCaptain where
  getAbilities (TheCaptain x) =
    [restrictedAbility x 1 (EachUndefeatedInvestigator $ at_ "The White Ship") parleyAction_]

instance RunMessage TheCaptain where
  runMessage msg a@(TheCaptain attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseOne
        iid
        [ SkillLabel s [parley iid (attrs.ability 1) iid s MaxAlarmLevelCalculation]
        | s <- [#willpower, #intellect]
        ]
      pure a
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      actId <- selectJust AnyAct
      push $ AdvanceAct actId (toSource attrs) #other
      pure a
    _ -> TheCaptain <$> lift (runMessage msg attrs)
