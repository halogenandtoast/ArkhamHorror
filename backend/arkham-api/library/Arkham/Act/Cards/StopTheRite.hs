module Arkham.Act.Cards.StopTheRite (stopTheRite) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Trait (Trait (Cultist))
import Arkham.Treachery.Cards qualified as Treacheries

newtype StopTheRite = StopTheRite ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stopTheRite :: ActCard StopTheRite
stopTheRite = act (2, A) StopTheRite Cards.stopTheRite Nothing

instance HasAbilities StopTheRite where
  getAbilities = actAbilities \a ->
    [ restricted a 1 (exists $ TreacheryAttachedToLocation YourLocation <> treacheryIs Treacheries.fire1)
        $ freeTrigger (GroupClueCost (PerPlayer 1) Anywhere)
    , restricted
        a
        2
        (exists $ EnemyWithTrait Cultist <> not_ (EnemyAt $ locationIs Locations.undergroundCistern))
        $ forced
        $ RoundEnds #when
    , mkAbility a 3 $ Objective $ forced $ ifEnemyDefeatedMatch (EnemyWithTitle "Elokoss")
    ]

instance RunMessage StopTheRite where
  runMessage msg a@(StopTheRite attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectEach
        (TreacheryAttachedToLocation (locationWithInvestigator iid) <> treacheryIs Treacheries.fire1)
        $ toDiscardBy iid (attrs.ability 1)
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      undergroundCistern <- selectJust $ locationIs Locations.undergroundCistern
      cultists <- select $ EnemyWithTrait Cultist <> not_ (EnemyAt $ LocationWithId undergroundCistern)
      leadChooseOneAtATimeM $ targets cultists \x -> moveTowards (attrs.ability 2) x undergroundCistern
      pure a
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    _ -> StopTheRite <$> liftRunMessage msg attrs
