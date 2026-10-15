module Arkham.Homebrew.TheFallOfTheHouseOfUsher.Acts.TheDisturbedProprietor (theDisturbedProprietor) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.Helpers (unDeck)
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Helpers.Scenario (getEncounterDeck)
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Assets qualified as Assets
import Arkham.Matcher

newtype TheDisturbedProprietor = TheDisturbedProprietor ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theDisturbedProprietor :: ActCard TheDisturbedProprietor
theDisturbedProprietor = act (1, A) TheDisturbedProprietor Cards.theDisturbedProprietor Nothing

{- | "randomly deal 1 facedown underneath locations in the following order until
each card is underneath a location". Both faces of a room print the same name,
so the order survives a collapse.
-}
dealOrder :: [LocationMatcher]
dealOrder =
  map
    LocationWithTitle
    [ "Usher Family Crypt"
    , "Banquet Hall"
    , "Wine Cellar"
    , "Manor Chapel"
    , "Portrait Gallery"
    , "Private Library"
    , "Grand Hall"
    , "Roderick's Studio"
    ]

instance HasAbilities TheDisturbedProprietor where
  getAbilities = actAbilities1 \a ->
    onlyOnce
      $ restricted
        a
        1
        (exists $ assetIs Assets.roderickUsher <> AssetWithClues (AtLeast $ PerPlayer 3))
      $ Objective
      $ forced AnyWindow

instance RunMessage TheDisturbedProprietor where
  runMessage msg a@(TheDisturbedProprietor attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      selectEach (assetIs Assets.roderickUsher) removeFromGame
      shuffleEncounterDiscardBackIn
      -- The deal reads off the top of the encounter deck, so it waits for the shuffle.
      doStep 1 msg
      advanceActDeck attrs
      pure a
    DoStep 1 (AdvanceAct (isSide B attrs -> True) _ _) -> do
      total <- (* 2) <$> getPlayerCount
      madeline <- getSetAsideCard Assets.madelineUsher
      top <- take (total - 1) . unDeck <$> getEncounterDeck
      for_ top obtainCard
      dealt <- traverse (setFacedown True) =<< shuffle (madeline : map toCard top)
      rooms <- catMaybes <$> traverse selectOne dealOrder
      for_ (zip rooms dealt) \(room, card) -> placeUnderneath room [card]
      pure a
    _ -> TheDisturbedProprietor <$> liftRunMessage msg attrs
