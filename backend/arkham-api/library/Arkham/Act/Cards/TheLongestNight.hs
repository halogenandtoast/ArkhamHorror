module Arkham.Act.Cards.TheLongestNight (theLongestNight) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (..))
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.ScenarioLogKey
import Arkham.Scenarios.TheLongestNight.Helpers
import Arkham.Token

newtype TheLongestNight = TheLongestNight ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theLongestNight :: ActCard TheLongestNight
theLongestNight = act (1, A) TheLongestNight Cards.theLongestNight Nothing

instance HasAbilities TheLongestNight where
  getAbilities = actAbilities \a ->
    [ restricted a 1 (DuringTurn You) $ FastAbility (ClueCost $ Static 1)
    , mkAbility a 2 $ ActionAbility #resign Nothing (ActionCost 1)
    ]

instance RunMessage TheLongestNight where
  runMessage msg a@(TheLongestNight attrs) = runQueueT $ scenarioI18n $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      lid <- selectJust $ locationWithInvestigator iid
      connected <- select $ connectedTo (LocationWithId lid)
      hasDecoy <- lid <=~> LocationWithHorror (atLeast 1)
      hasTrap <- lid <=~> LocationWithDamage (atLeast 1)
      canHaveDecoys <- lid <=~> LocationWithoutModifier CannotHaveDecoys
      canHaveTraps <- lid <=~> LocationWithoutModifier CannotHaveTraps
      chooseOneM iid do
        when (notNull connected) do
          labeled' "placeBarrier" do
            chooseTargetM iid connected \toLid ->
              push $ ScenarioCountIncrementBy (Barriers lid toLid) 1
        when (canHaveDecoys && not hasDecoy) do
          labeled' "placeDecoy" do
            placeDecoy attrs lid
        when (canHaveTraps && not hasTrap) do
          labeled' "placeTrap" do
            placeTrap attrs lid
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      resign iid
      doStep 2 msg
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      whenM (selectNone UneliminatedInvestigator) $ push R3
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      captives <- selectJust $ assetIs Assets.theCaptives
      damage <- fieldMap AssetTokens (countTokens Damage) captives
      push $ if damage <= 4 then R1 else R2
      pure a
    _ -> TheLongestNight <$> liftRunMessage msg attrs
