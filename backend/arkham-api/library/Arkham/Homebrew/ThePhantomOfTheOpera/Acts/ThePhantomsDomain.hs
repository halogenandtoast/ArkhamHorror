module Arkham.Homebrew.ThePhantomOfTheOpera.Acts.ThePhantomsDomain (thePhantomsDomain) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Agenda.Types (Field (AgendaDeckId))
import Arkham.Campaigns.TheScarletKeys.Concealed.Matcher
import Arkham.Campaigns.TheScarletKeys.Helpers (placeConcealedCard, whenExposed)
import Arkham.Helpers.Agenda (getCurrentAgenda)
import Arkham.Helpers.Window (getAttackDetails)
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.ThePhantomOfTheOpera.Import
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Placement
import Arkham.Projection

newtype ThePhantomsDomain = ThePhantomsDomain ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePhantomsDomain :: ActCard ThePhantomsDomain
thePhantomsDomain = act (3, A) ThePhantomsDomain Cards.thePhantomsDomain Nothing

instance HasAbilities ThePhantomsDomain where
  getAbilities = actAbilities \a ->
    [ restricted a 1 (exists $ enemyIs Enemies.thePhantom)
        $ forced
        $ CampaignEvent #after (Just You) "exposed[decoy]"
    , scenarioI18n
        $ withI18nTooltip "thePhantomsDomain.misdirect"
        $ mkAbility a 2
        $ triggered
          (EnemyWouldAttack #when Anyone (CancelableEnemyAttack AnyEnemyAttack) (enemyIs Enemies.thePhantom))
          (clueCost 1)
    , mkAbility a 3 $ forced $ whenExposed Enemies.thePhantom
    ]

instance RunMessage ThePhantomsDomain where
  runMessage msg a@(ThePhantomsDomain attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectForMaybeM (enemyIs Enemies.thePhantom) \phantom ->
        initiateEnemyAttack phantom (attrs.ability 1) iid
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 (getAttackDetails -> details) _ -> do
      cancelAttack (attrs.ability 2) details
      pure a
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      placeTokens (attrs.ability 3) attrs #clue 1
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      lead <- getLead
      lair <- selectJust $ locationIs Locations.thePhantomsLair
      moveAllTo attrs lair
      selectEach (EnemyAt Anywhere <> not_ (EnemyWithPlacement InTheShadows)) \eid ->
        enemyMoveTo attrs eid lair
      selectEach (ConcealedCardAt $ not_ (LocationWithId lair)) \c ->
        placeConcealedCard lead c.id (AtLocation lair)
      selectEach (LocationWithTitle "Paris Sewers") removeLocation
      placeSetAsideLocations_ [Locations.hiddenParlor, Locations.organChamber]
      push $ SetLayout ["hourglass plus star"]
      topUpConcealedDecoys
      doStep 1 msg
      agendaId <- getCurrentAgenda
      deckId <- field AgendaDeckId agendaId
      push $ AdvanceAgendaDeck deckId (toSource attrs)
      advanceActDeck attrs
      pure a
    DoStep 1 (AdvanceAct (isSide B attrs -> True) _ _) -> do
      redistributeConcealed $ locationIs Locations.thePhantomsLair
      pure a
    _ -> ThePhantomsDomain <$> liftRunMessage msg attrs
