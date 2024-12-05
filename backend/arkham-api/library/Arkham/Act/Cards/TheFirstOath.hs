module Arkham.Act.Cards.TheFirstOath (TheFirstOath (..), theFirstOath) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Key
import Arkham.Keyword (Keyword (Aloof))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.ScenarioLogKey
import Arkham.Trait (Trait (Obstacle, Suspect))
import Arkham.Treachery.Cards qualified as Treacheries

newtype TheFirstOath = TheFirstOath ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theFirstOath :: ActCard TheFirstOath
theFirstOath = act (1, A) TheFirstOath Cards.theFirstOath Nothing

instance HasModifiersFor TheFirstOath where
  getModifiersFor (TheFirstOath a) = do
    n <- perPlayer 1
    suspects <- modifySelect a (EnemyWithTrait Suspect) [HealthModifier n, RemoveKeyword Aloof]
    investigators <- modifySelect a Anyone [CannotParleyWith $ EnemyWithTrait Suspect]
    pure $ suspects <> investigators

instance HasAbilities TheFirstOath where
  getAbilities (TheFirstOath x) =
    extend
      x
      [ restricted x 1 (exists $ TreacheryWithTrait Obstacle <> TreacheryAt YourLocation)
          $ FastAbility
          $ OrCost
          $ map SpendKeyCost [BlueKey, RedKey, WhiteKey, YellowKey]
      , restricted
          x
          2
          ( EachUndefeatedInvestigator (at_ $ locationIs Locations.grandEntryway)
              <> Remembered UnlockedTheEntranceToTheCaves
          )
          $ Objective
          $ forced AnyWindow
      ]

instance RunMessage TheFirstOath where
  runMessage msg a@(TheFirstOath attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      selectOne (EnemyWithTrait Suspect) >>= traverse_ removeFromGame
      selectEach (not_ $ locationIs Locations.grandEntryway) removeLocation
      placeRandomLocationGroup "tidalTunnel"
        =<< getSetAsideCardsMatching (CardWithTitle "Tidal Tunnel")

      push
        $ SetLayout
          [ ".            tidalTunnel1 tidalTunnel1  tidalTunnel2  tidalTunnel2 ."
          , ".            tidalTunnel1 tidalTunnel1  tidalTunnel2  tidalTunnel2 ."
          , "tidalTunnel3 tidalTunnel3 .             .             tidalTunnel4 tidalTunnel4"
          , "tidalTunnel3 tidalTunnel3 grandEntryway grandEntryway tidalTunnel4 tidalTunnel4"
          , "tidalTunnel5 tidalTunnel5 grandEntryway grandEntryway tidalTunnel6 tidalTunnel6"
          , "tidalTunnel5 tidalTunnel5 .             .             tidalTunnel6 tidalTunnel6"
          , ".            tidalTunnel7 tidalTunnel7  lairOfDagon   lairOfDagon ."
          , ".            tidalTunnel7 tidalTunnel7  lairOfDagon   lairOfDagon ."
          ]

      doStep 1 msg

      shuffleSetAsideIntoEncounterDeck [Treacheries.syzygy, Treacheries.tidalAlignment]
      shuffleEncounterDiscardBackIn

      advanceActDeck attrs
      pure a
    DoStep 1 (AdvanceAct (isSide B attrs -> True) _ _) -> do
      selectEach Anywhere increaseThisFloodLevel
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectEach (TreacheryAt (locationWithInvestigator iid) <> TreacheryWithTrait Obstacle)
        $ toDiscardBy iid (attrs.ability 1)
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    _ -> TheFirstOath <$> liftRunMessage msg attrs
