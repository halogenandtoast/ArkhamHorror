module Arkham.Act.Cards.FalseStepV2 (falseStepV2) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Window (getTotalDamage)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Placement
import Arkham.Strategy
import Arkham.Trait (Trait (Coterie, Havana, Outsider))

newtype FalseStepV2 = FalseStepV2 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

falseStepV2 :: ActCard FalseStepV2
falseStepV2 = act (1, A) FalseStepV2 Cards.falseStepV2 (groupClueCost (PerPlayer 2))

instance HasAbilities FalseStepV2 where
  getAbilities = actAbilities \a ->
    [ restricted a 1 (exists $ EnemyWithPlacement InTheShadows) $ forced $ PhaseEnds #when #enemy
    , restricted a 2 (exists $ locationIs Locations.cafeLunaCoterieHaunt <> LocationWithAnyClues)
        $ freeReaction
        $ EnemyTakeDamage #after AnyDamageEffect (EnemyWithTrait Coterie) (atLeast 2) AnySource
    ]

instance RunMessage FalseStepV2 where
  runMessage msg a@(FalseStepV2 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      lead <- getLead
      assignDamageWithStrategy
        lead
        (attrs.ability 1)
        (AmongInvestigators $ at_ $ locationIs Locations.cafeLunaCoterieHaunt)
        =<< selectCount (EnemyWithPlacement InTheShadows)
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 (getTotalDamage -> n) _ -> do
      discoverAtYourLocation NotInvestigate iid (attrs.ability 2) (n `div` 2)
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      shuffleEncounterDiscardBackIn
      shuffleSetAsideIntoEncounterDeck (card_ $ #enemy <> CardWithTrait Outsider)
      selectEach (enemyIs Enemies.desiderioDelgadoAlvarez106 <> not_ (EnemyWithPlacement InTheShadows)) \eid -> do
        place eid InTheShadows
        lead <- getLead
        resolveConcealed lead eid
      placeSetAsideLocationsMatching_ (CardWithTrait Havana)
      do_ msg
      advanceActDeck attrs
      pure a
    Do (AdvanceAct (isSide B attrs -> True) _ _) -> do
      -- we need desiderio's concealed cards to be in play
      concealed <- shuffle =<< selectMap (.id) ConcealedCardAny
      locations <- select $ not_ $ locationIs Locations.cafeLunaCoterieHaunt
      distributeEvenlyBetween concealed locations
      pure a
    _ -> FalseStepV2 <$> liftRunMessage msg attrs
