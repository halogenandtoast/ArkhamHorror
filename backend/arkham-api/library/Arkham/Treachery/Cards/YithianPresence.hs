module Arkham.Treachery.Cards.YithianPresence (yithianPresence, YithianPresence (..)) where

import Arkham.Ability
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Placement
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype YithianPresence = YithianPresence TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yithianPresence :: TreacheryCard YithianPresence
yithianPresence = treachery YithianPresence Cards.yithianPresence

instance HasModifiersFor YithianPresence where
  getModifiersFor (YithianPresence a) = case a.placement of
    InThreatArea iid -> do
      yithianPresent <- selectAny $ EnemyWithTrait Yithian <> enemyAtLocationWith iid
      mlid <- selectOne $ locationWithInvestigator iid
      modifiedWhen_ a yithianPresent iid
        $ ( CannotTriggerAbilityMatching AbilityOnEncounterCard
              : [CannotInvestigateLocation lid | lid <- maybeToList mlid]
          )
    _ -> pure mempty

instance HasAbilities YithianPresence where
  getAbilities (YithianPresence a) =
    [restrictedAbility a 1 OnSameLocation $ actionAbilityWithCost $ HandDiscardCost 2 #any]

instance RunMessage YithianPresence where
  runMessage msg t@(YithianPresence attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> YithianPresence <$> liftRunMessage msg attrs
