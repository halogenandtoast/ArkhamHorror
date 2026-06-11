module Arkham.Location.Cards.TickTockClubPresent (tickTockClubPresent) where

import Arkham.Ability
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.MachinationsThroughTime.Helpers
import Arkham.Token qualified as Token

newtype TickTockClubPresent = TickTockClubPresent LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tickTockClubPresent :: LocationCard TickTockClubPresent
tickTockClubPresent = location TickTockClubPresent Cards.tickTockClubPresent 4 (PerPlayer 3)

instance HasAbilities TickTockClubPresent where
  getAbilities (TickTockClubPresent a) =
    extendRevealed1 a
      $ restricted a 1 Here
      $ actionAbilityWithCost (SpendTokenCost Token.Time (TargetIs $ toTarget a))

instance RunMessage TickTockClubPresent where
  runMessage msg l@(TickTockClubPresent attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      agenda <- selectJust AnyAgenda
      removeDoom (attrs.ability 1) agenda 1
      thomas <-
        selectOne $ AssetWithTitle "Thomas Corrigan" <> AssetAt (be attrs) <> AssetReady
      for_ thomas \thomas' -> do
        tickTockClub <- selectOne $ locationIs Cards.tickTockClubFuture
        for_ tickTockClub \club -> do
          chooseOneM iid $ withI18n do
            labeled' "skip" nothing
            scenarioI18n $ labeled' "tickTockClub.placeTime" do
              exhaustThis thomas'
              placeTokens (attrs.ability 1) club Token.Time 1
      pure l
    _ -> TickTockClubPresent <$> liftRunMessage msg attrs
