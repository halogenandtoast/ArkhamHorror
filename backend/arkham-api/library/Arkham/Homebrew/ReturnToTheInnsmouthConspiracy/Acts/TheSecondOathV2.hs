module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Acts.TheSecondOathV2 (theSecondOathV2) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Enemy.CardDefs.TheInnsmouthConspiracy.TheLairOfDagon qualified as Enemies
import Arkham.Helpers.Agenda
import Arkham.Helpers.ChaosBag
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Treacheries qualified as HBTreacheries
import Arkham.Key
import Arkham.Keyword (Keyword (Aloof))
import Arkham.Location.CardDefs.TheInnsmouthConspiracy.TheLairOfDagon qualified as Locations
import Arkham.Location.FloodLevel
import Arkham.Matcher
import Arkham.ScenarioLogKey
import Arkham.Trait (Trait (Obstacle, Suspect))

newtype TheSecondOathV2 = TheSecondOathV2 ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSecondOathV2 :: ActCard TheSecondOathV2
theSecondOathV2 = act (2, A) TheSecondOathV2 Cards.theSecondOathV2 Nothing

instance HasModifiersFor TheSecondOathV2 where
  getModifiersFor (TheSecondOathV2 a) = do
    n <- perPlayer 1
    modifySelect a (EnemyWithTrait Suspect) [HealthModifier n, RemoveKeyword Aloof]
    modifySelect a Anyone [CannotParleyWith $ EnemyWithTrait Suspect]

instance HasAbilities TheSecondOathV2 where
  getAbilities (TheSecondOathV2 x)
    | onSide A x =
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
  getAbilities _ = []

instance RunMessage TheSecondOathV2 where
  runMessage msg a@(TheSecondOathV2 attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      lair <- placeSetAsideLocation Locations.lairOfDagon
      setThisFloodLevel lair FullyFlooded
      dagon <- getSetAsideCard Enemies.dagonDeepInSlumber
      createEnemyAt_ dagon lair
      n <- getCurrentAgendaStep
      when (n == 1) do
        x <- min 10 <$> getRemainingCurseTokens
        repeated x $ addChaosToken #curse
      when (n == 2) do
        x <- min 5 <$> getRemainingCurseTokens
        repeated x $ addChaosToken #curse
      -- v2: the set-aside Stirring in His Sleep joins the encounter deck.
      shuffleSetAsideIntoEncounterDeck [HBTreacheries.stirringInHisSleep]
      advanceActDeck attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectEach (TreacheryAt (locationWithInvestigator iid) <> TreacheryWithTrait Obstacle)
        $ toDiscardBy iid (attrs.ability 1)
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    _ -> TheSecondOathV2 <$> liftRunMessage msg attrs
