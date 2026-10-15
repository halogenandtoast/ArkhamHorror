module Arkham.Homebrew.TheFallOfTheHouseOfUsher.Acts.FleetingGlimpses (fleetingGlimpses) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (ActionCostModifier), modified_)
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.Helpers (ruinedLocation)
import Arkham.I18n
import Arkham.Location.Types (Field (LocationCardsUnderneath))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype FleetingGlimpses = FleetingGlimpses ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fleetingGlimpses :: ActCard FleetingGlimpses
fleetingGlimpses = act (2, A) FleetingGlimpses Cards.fleetingGlimpses Nothing

hasCardUnderneath :: Criterion
hasCardUnderneath = exists $ You <> at_ (LocationWithCardsUnderneath $ HasCard IsEncounterCard)

-- | "If you are at a [[Ruined]] location, reduce this ability's cost by 1 action."
instance HasModifiersFor FleetingGlimpses where
  getModifiersFor (FleetingGlimpses a) = do
    iids <- select $ InvestigatorAt ruinedLocation
    for_ iids \iid ->
      modified_ a (AbilityTarget iid (AbilityRef (toSource a) 1)) [ActionCostModifier (-1)]

instance HasAbilities FleetingGlimpses where
  getAbilities = actAbilities \a ->
    [ restricted a 1 hasCardUnderneath doubleActionAbility
    , restricted a 2 hasCardUnderneath $ actionAbilityWithCost (clueCost 1)
    , onlyOnce
        $ restricted a 3 (exists $ InEncounterDiscard <> basic (cardIs Assets.madelineUsher))
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage FleetingGlimpses where
  runMessage msg a@(FleetingGlimpses attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withLocationOf iid \loc -> do
        cards <- fieldMap LocationCardsUnderneath (take 1) loc
        revealed <- traverse (setFacedown False) cards
        for_ revealed (drawCard iid)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withLocationOf iid \loc -> do
        cards <- fieldMap LocationCardsUnderneath (take 1) loc
        revealed <- traverse (setFacedown False) cards
        focusCards revealed do
          chooseOneM iid $ withI18n $ labeled "continue" do
            unfocusCards
            for_ cards \card -> forTarget' card $ doStep 1 msg
      pure a
    ForTarget (CardIdTarget cid) (DoStep 1 (UseThisAbility _ (isSource attrs -> True) 2)) -> do
      card <- fetchCard cid
      void $ setFacedown True card
      pure a
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      underneath <- concatMapM (field LocationCardsUnderneath) =<< select Anywhere
      for_ underneath obtainCard
      revealed <- traverse (setFacedown False) underneath
      shuffleCardsIntoDeck Deck.EncounterDeck revealed
      shuffleEncounterDiscardBackIn

      tarn <- placeSetAsideLocation Locations.theTarn
      push $ SetLocationLabel tarn "theTarn"
      placeTokens attrs tarn #damage 2
      createSetAsideEnemy_ Enemies.theThingFromTheTarn tarn
      advanceActDeck attrs
      pure a
    _ -> FleetingGlimpses <$> liftRunMessage msg attrs
