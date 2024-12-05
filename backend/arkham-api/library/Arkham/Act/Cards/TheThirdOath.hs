module Arkham.Act.Cards.TheThirdOath (TheThirdOath (..), theThirdOath) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Act.Types (Field (..))
import Arkham.CampaignLogKey
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Key
import Arkham.Keyword (Keyword (Aloof))
import Arkham.Matcher
import Arkham.Trait (Trait (Obstacle, Suspect))

newtype TheThirdOath = TheThirdOath ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theThirdOath :: ActCard TheThirdOath
theThirdOath = act (3, A) TheThirdOath Cards.theThirdOath Nothing

instance HasModifiersFor TheThirdOath where
  getModifiersFor (TheThirdOath a) = do
    n <- perPlayer 1
    enemies <- modifySelect a (EnemyWithTrait Suspect) [HealthModifier n, RemoveKeyword Aloof]
    investigators <- modifySelect a Anyone [CannotParleyWith $ EnemyWithTrait Suspect]
    pure $ enemies <> investigators

instance HasAbilities TheThirdOath where
  getAbilities (TheThirdOath x) =
    extend
      x
      [ restricted x 1 (exists $ TreacheryWithTrait Obstacle <> TreacheryAt YourLocation)
          $ FastAbility
          $ OrCost
          $ map SpendKeyCost [BlueKey, RedKey, WhiteKey, YellowKey]
      , restricted x 2 (HasCalculation (ActFieldCalculation x.id ActClues) (AtLeast $ PerPlayer 3))
          $ Objective
          $ forced AnyWindow
      ]

instance RunMessage TheThirdOath where
  runMessage msg a@(TheThirdOath attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      record TheOrdersRitualWasDisrupted
      slumbering <- selectAny $ IncludeOmnipotent $ enemyIs Enemies.dagonDeepInSlumber
      record
        $ if slumbering
          then DagonStillSlumbers
          else DagonHasAwakened
      push R1
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectEach (TreacheryAt (locationWithInvestigator iid) <> TreacheryWithTrait Obstacle)
        $ toDiscardBy iid (attrs.ability 1)
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    _ -> TheThirdOath <$> liftRunMessage msg attrs
