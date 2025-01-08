module Arkham.Act.Cards.TheSecondOath (TheSecondOath (..), theSecondOath) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Agenda
import Arkham.Helpers.ChaosBag
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers
import Arkham.Key
import Arkham.Keyword (Keyword (Aloof))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.ScenarioLogKey
import Arkham.Trait (Trait (Obstacle, Suspect))

newtype TheSecondOath = TheSecondOath ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSecondOath :: ActCard TheSecondOath
theSecondOath = act (2, A) TheSecondOath Cards.theSecondOath Nothing

instance HasModifiersFor TheSecondOath where
  getModifiersFor (TheSecondOath a) = do
    n <- perPlayer 1
    enemies <- modifySelect a (EnemyWithTrait Suspect) [HealthModifier n, RemoveKeyword Aloof]
    investigators <- modifySelect a Anyone [CannotParleyWith $ EnemyWithTrait Suspect]
    pure $ enemies <> investigators

instance HasAbilities TheSecondOath where
  getAbilities (TheSecondOath x) =
    extend
      x
      [ restricted x 1 (exists $ TreacheryWithTrait Obstacle <> TreacheryAt YourLocation)
          $ FastAbility
          $ OrCost
          $ map SpendKeyCost [BlueKey, RedKey, WhiteKey, YellowKey]
      , restricted x 2 (Remembered UnlockedTheFinalDepths)
          $ Objective
          $ forced AnyWindow
      ]

instance RunMessage TheSecondOath where
  runMessage msg a@(TheSecondOath attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      lair <- placeSetAsideLocation Locations.lairOfDagon
      dagon <- getSetAsideCard Enemies.dagonDeepInSlumber
      createEnemyAt_ dagon lair
      n <- getCurrentAgendaStep
      when (n == 1) do
        x <- min 10 <$> getRemainingCurseTokens
        repeated x $ addChaosToken #curse
      when (n == 2) do
        x <- min 5 <$> getRemainingCurseTokens
        repeated x $ addChaosToken #curse
      advanceActDeck attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectEach (TreacheryAt (locationWithInvestigator iid) <> TreacheryWithTrait Obstacle)
        $ toDiscardBy iid (attrs.ability 1)
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    _ -> TheSecondOath <$> liftRunMessage msg attrs
