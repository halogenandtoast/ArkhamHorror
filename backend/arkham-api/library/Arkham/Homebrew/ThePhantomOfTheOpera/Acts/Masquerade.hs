module Arkham.Homebrew.ThePhantomOfTheOpera.Acts.Masquerade (masquerade) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheScarletKeys.Concealed.Matcher
import Arkham.Campaigns.TheScarletKeys.Helpers (placeConcealedCard)
import Arkham.Card.CardCode
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.Query (getInvestigators)
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.ThePhantomOfTheOpera.Import
import Arkham.Homebrew.ThePhantomOfTheOpera.Traits
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Placement

newtype Masquerade = Masquerade ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

masquerade :: ActCard Masquerade
masquerade = act (1, A) Masquerade Cards.masquerade (groupClueCost $ PerPlayer 1)

grandFoyer :: LocationMatcher
grandFoyer = LocationWithTitle "Grand Foyer"

{- | The exposing investigator is the only seat offered the reaction, so "the location
where The Phantom's mini-card was exposed" is their location: the mini-card is already
unplaced by the time the window opens.
-}
whenPhantomExposed :: WindowMatcher
whenPhantomExposed =
  CampaignEvent #when (Just You) $ "exposed[" <> unCardCode Enemies.thePhantom.cardCode <> "]"

instance HasAbilities Masquerade where
  getAbilities = actAbilities1 \a ->
    scenarioI18n
      $ withI18nTooltip "masquerade.unmasked"
      $ mkAbility a 1
      $ freeReaction whenPhantomExposed

instance RunMessage Masquerade where
  runMessage msg a@(Masquerade attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withLocationOf iid \lid -> placeTokens (attrs.ability 1) lid #clue 1
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      investigators <- getInvestigators
      persian <- getSetAsideCard Assets.thePersian
      gameModifier ScenarioSource persian DoNotTakeUpSlots
      leadChooseOneM $ scenarioI18n $ scope "masquerade" do
        questionLabeled "thePersian"
        questionLabeledCard Assets.thePersian
        portraits investigators (`takeControlOfSetAsideAsset` persian)
      doStep 1 msg
      advanceActDeck attrs
      pure a
    DoStep 1 msg'@(AdvanceAct (isSide B attrs -> True) _ _) -> do
      lead <- getLead
      foyers <- select $ grandFoyer <> RevealedLocation
      chooseOneM lead $ scenarioI18n $ scope "masquerade" do
        questionLabeled "chooseGrandFoyer"
        targets foyers \lid -> forTarget_ lid (DoStep 2 msg')
      pure a
    ForTarget (LocationTarget lid) (DoStep 2 msg'@(AdvanceAct (isSide B attrs -> True) _ _)) -> do
      lead <- getLead
      moveAllTo attrs lid
      selectEach (EnemyAt Anywhere <> not_ (EnemyWithPlacement InTheShadows)) \eid ->
        enemyMoveTo attrs eid lid
      selectEach (ConcealedCardAt $ not_ (LocationWithId lid)) \c ->
        placeConcealedCard lead c.id (AtLocation lid)
      selectEach (grandFoyer <> not_ (LocationWithId lid)) removeLocation
      setLocationLabel lid "grandFoyer"
      placeSetAsideLocations_ [Locations.auditorium, Locations.backstage, Locations.boxFive]
      push $ SetLayout [". heart .", "grandFoyer . moon", ". square ."]
      topUpConcealedDecoys
      doStep 3 msg'
      pure a
    DoStep 3 (AdvanceAct (isSide B attrs -> True) _ _) -> do
      redistributeConcealed $ LocationWithTrait OperaHouse
      pure a
    _ -> Masquerade <$> liftRunMessage msg attrs
