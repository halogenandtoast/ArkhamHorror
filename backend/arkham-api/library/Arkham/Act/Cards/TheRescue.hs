module Arkham.Act.Cards.TheRescue (TheRescue (..), theRescue) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Keyword (Keyword (Aloof))
import Arkham.Matcher
import Arkham.Scenarios.TheVanishingOfElinaHarper.Helpers

newtype TheRescue = TheRescue ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theRescue :: ActCard TheRescue
theRescue = act (1, A) TheRescue Cards.theRescue Nothing

instance HasModifiersFor TheRescue where
  getModifiersFor (TheRescue a) = do
    kidnapper <- toCardCode <$> getKidnapper
    n <- perPlayer 1
    enemies <- modifySelect a (EnemyIs kidnapper) [RemoveKeyword Aloof, HealthModifier n]
    investigators <- modifySelect a Anyone [CannotParleyWith $ EnemyIs kidnapper]
    pure $ enemies <> investigators

instance HasAbilities TheRescue where
  getAbilities (TheRescue attrs) = extend attrs [restrictedAbility attrs 1 criteria $ Objective $ ForcedAbility AnyWindow]
   where
    criteria =
      oneOf
        [ PlayerCountIs 1 <> oneOf [kidnapperInVictoryDisplay, elinasLocationHasNoClues]
        , kidnapperInVictoryDisplay <> elinasLocationHasNoClues
        ]
    kidnapperInVictoryDisplay = exists $ VictoryDisplayCardMatch $ CardIdentifiedByScenarioMetaKey "kidnapper"
    elinasLocationHasNoClues =
      exists
        $ LocationWithoutClues
        <> LocationWithCardsUnderneath (HasCard $ cardIs Assets.elinaHarperKnowsTooMuch)

instance RunMessage TheRescue where
  runMessage msg a@(TheRescue attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      kidnapper <- toCardCode <$> getKidnapper

      push
        $ if
          | kidnapper == Enemies.brianBurnhamWantsOut.cardCode -> R2
          | kidnapper == Enemies.otheraGilmanProprietessOfTheHotel.cardCode -> R3
          | kidnapper == Enemies.joyceLittleBookshopOwner.cardCode -> R4
          | kidnapper == Enemies.barnabasMarshTheChangeIsUponHim.cardCode -> R5
          | kidnapper == Enemies.zadokAllenDrunkAndDisorderly.cardCode -> R6
          | kidnapper == Enemies.robertFriendlyDisgruntledDockworker.cardCode -> R7
          | otherwise -> error "Invalid kidnapper"

      pure a
    _ -> TheRescue <$> liftRunMessage msg attrs
