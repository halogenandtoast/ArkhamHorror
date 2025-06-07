module Arkham.Act.Cards.EscapeTheRuins (escapeTheRuins) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Scenario
import Arkham.Matcher
import Arkham.Trait

newtype EscapeTheRuins = EscapeTheRuins ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

escapeTheRuins :: ActCard EscapeTheRuins
escapeTheRuins = act (3, A) EscapeTheRuins Cards.escapeTheRuins Nothing

instance HasModifiersFor EscapeTheRuins where
  getModifiersFor (EscapeTheRuins a) = do
    n <- getVengeanceInVictoryDisplay
    modifySelectWhen a (n >= 3) (EnemyWithTrait Serpent) [EnemyEvade 1]

instance HasAbilities EscapeTheRuins where
  getAbilities = actAbilities \x ->
    [restricted x 1 AllUndefeatedInvestigatorsResigned $ Objective $ forced AnyWindow]

instance RunMessage EscapeTheRuins where
  runMessage msg a@(EscapeTheRuins attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      resignedWithRelicOfAges <- resignedWith Assets.relicOfAgesADeviceOfSomeSort
      push $ if resignedWithRelicOfAges then R1 else R3
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    _ -> EscapeTheRuins <$> liftRunMessage msg attrs
