module Arkham.Agenda.Cards.TheOnslaught (theOnslaught) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (..))
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Deck
import Arkham.Draw.Types
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query (getLead)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Trait (Trait (Resident))
import Arkham.Window (Window (windowType), getBatchId)
import Arkham.Window qualified as Window

newtype TheOnslaught = TheOnslaught AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theOnslaught :: AgendaCard TheOnslaught
theOnslaught = agenda (1, A) TheOnslaught Cards.theOnslaught (Static 10)

getWindowDoom :: [Window] -> Int
getWindowDoom ((windowType -> Window.WouldPlaceDoom _ _ n) : _) = n
getWindowDoom (_ : rest) = getWindowDoom rest
getWindowDoom [] = error "No doom amount found"

instance HasModifiersFor TheOnslaught where
  getModifiersFor (TheOnslaught a) = modifySelf a [OtherDoomSubtracts]

instance HasAbilities TheOnslaught where
  getAbilities (TheOnslaught a)
    | onSide A a =
        [ restricted a 1 (exists $ assetIs Assets.theCaptives)
            $ forced
            $ WouldPlaceDoomCounter #when SourceIsCardEffect (targetIs a)
        , forcedAbility a 2 $ PlacedDoomCounter #after AnySource (targetIs a)
        ]
  getAbilities _ = []

instance RunMessage TheOnslaught where
  runMessage msg a@(TheOnslaught attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (getBatchId &&& getWindowDoom -> (batchId, n)) _ -> do
      captives <- selectJust $ assetIs Assets.theCaptives
      cancelBatch batchId
      placeDoom attrs captives n
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      eachInvestigator \iid ->
        drawEncounterCardsEdit iid attrs 1 \d -> d {cardDrawDeck = ScenarioDeckByKey EnemyDeck}
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      lead <- getLead
      residents <- select $ AssetWithTrait Resident <> AssetControlledBy Anyone
      chooseOneM lead $ campaignI18n do
        labeled' "theOnslaught.eachUndefeatedInvestigatorSuffersMentalTrauma" do
          eachInvestigator (`sufferMentalTrauma` 1)
        when (notNull residents) do
          labeled' "theOnslaught.chooseResidentToSacrifice" do
            chooseTargetM lead residents \aid -> do
              cardDef <- field AssetCardDef aid
              for_ (residentFromCardDef cardDef) \resident -> do
                crossOut (crossedOutKey resident)
                record (sacrificedKey resident)
              removeAsset aid
      actId <- selectJust AnyAct
      push $ AdvanceAct actId (toSource attrs) #other
      pure a
    _ -> TheOnslaught <$> liftRunMessage msg attrs
