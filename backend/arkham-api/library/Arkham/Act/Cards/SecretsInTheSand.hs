module Arkham.Act.Cards.SecretsInTheSand (secretsInTheSand) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheForgottenAge.Helpers (exploreAction_, runExplore)
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Query (getSetAsideCardMaybe)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Trait (Trait (Cairo, Expedition))

newtype SecretsInTheSand = SecretsInTheSand ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

secretsInTheSand :: ActCard SecretsInTheSand
secretsInTheSand = act (2, A) SecretsInTheSand Cards.secretsInTheSand (groupClueCost $ PerPlayer 3)

instance HasModifiersFor SecretsInTheSand where
  getModifiersFor (SecretsInTheSand a) = do
    modifySelect a (LocationWithTrait Cairo) [CannotPlaceClues]

instance HasAbilities SecretsInTheSand where
  getAbilities (SecretsInTheSand a) =
    withBaseAbilities
      a
      [restricted a 1 (youExist $ InvestigatorAt $ LocationWithTrait Expedition) exploreAction_]

instance RunMessage SecretsInTheSand where
  runMessage msg a@(SecretsInTheSand attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      runExplore iid (attrs.ability 1)
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      dreamers <- perPlayer 1
      selectEach (LocationWithTrait Cairo) \loc -> placeTokens attrs loc #damage dreamers
      streetsOfCairo <- selectJust $ locationIs Locations.streetsOfCairo
      whenJustM (getSetAsideCardMaybe Enemies.neith) \neith ->
        createEnemyAt_ neith streetsOfCairo
      advanceActDeck attrs
      pure a
    _ -> SecretsInTheSand <$> liftRunMessage msg attrs
