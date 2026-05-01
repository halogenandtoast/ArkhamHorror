module Arkham.Act.Cards.SearchTheSewers (searchTheSewers) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Treacheries

newtype SearchTheSewers = SearchTheSewers ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

searchTheSewers :: ActCard SearchTheSewers
searchTheSewers = act (1, A) SearchTheSewers Cards.searchTheSewers Nothing

instance HasAbilities SearchTheSewers where
  getAbilities = actAbilities \a ->
    [ restricted a 1 (exists $ You <> at_ (LocationWithTitle "Underground Cistern"))
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage SearchTheSewers where
  runMessage msg a@(SearchTheSewers attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      undergroundCistern <- selectJust $ locationIs Locations.undergroundCistern
      elokoss <- getSetAsideCard Enemies.elokossFaintEmbers
      createEnemyAt_ elokoss undergroundCistern

      fireCard <- getSetAsideCard Treacheries.fire1
      miid <- selectOne $ InvestigatorAt (LocationWithId undergroundCistern)
      for_ miid (`drawCard` fireCard)

      doStep 1 msg

      placeSetAsideLocation_ Locations.sluiceControl

      advanceActDeck attrs
      pure a
    DoStep 1 (AdvanceAct (isSide B attrs -> True) _ _) -> do
      remainingFires <- getSetAsideCardsMatching $ cardIs Treacheries.fire1
      queensKnight <- maybeToList <$> getSetAsideCardMaybe Enemies.queensKnight
      heraldOfFlame <- maybeToList <$> getSetAsideCardMaybe Enemies.heraldOfFlame
      shuffleCardsIntoDeck Deck.EncounterDeck $ remainingFires <> queensKnight <> heraldOfFlame
      pure a
    _ -> SearchTheSewers <$> liftRunMessage msg attrs
