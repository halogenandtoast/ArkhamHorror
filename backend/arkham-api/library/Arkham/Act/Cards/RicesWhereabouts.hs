module Arkham.Act.Cards.RicesWhereabouts (ricesWhereabouts) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Agenda (whenCurrentAgendaStepIs)
import Arkham.Helpers.Campaign
import Arkham.Helpers.Query
import Arkham.Helpers.Window (cardsDiscarded)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Placement

newtype RicesWhereabouts = RicesWhereabouts ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ricesWhereabouts :: ActCard RicesWhereabouts
ricesWhereabouts = act (2, A) RicesWhereabouts Cards.ricesWhereabouts Nothing

instance HasAbilities RicesWhereabouts where
  getAbilities (RicesWhereabouts x) =
    [ mkAbility x 1 $ actionAbilityWithCost (clueCost 1)
    , mkAbility x 2 $ forced $ Discarded #when (Just You) AnySource (basic $ cardIs Assets.jazzMulligan)
    , mkAbility x 3 $ Objective $ forced $ TookControlOfAsset #when You (assetIs Assets.jazzMulligan)
    ]

instance RunMessage RicesWhereabouts where
  runMessage msg a@(RicesWhereabouts attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      playerCount <- getPlayerCount
      discardTopOfEncounterDeck iid (attrs.ability 1) $ if playerCount == 1 then 10 else 5
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 (cardsDiscarded -> cards) _ -> do
      for_ (filterCards Assets.jazzMulligan cards) (drawCard iid)
      pure a
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      advanceVia #other attrs (attrs.ability 3)
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      alchemyLabs <- placeLocationIfNotInPlay Locations.alchemyLabs
      whenCurrentAgendaStepIs (<= 2) do
        createEnemyAtLocationMatching_ Enemies.theExperiment (LocationWithId alchemyLabs)
      whenM (any (`elem` ["02062", "51015"]) <$> getCompletedScenarios) do
        createAssetAt_ Assets.alchemicalConcoction (AtLocation alchemyLabs)
      advanceActDeck attrs
      pure a
    _ -> RicesWhereabouts <$> liftRunMessage msg attrs
