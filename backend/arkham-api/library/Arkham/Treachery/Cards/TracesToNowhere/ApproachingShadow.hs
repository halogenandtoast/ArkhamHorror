module Arkham.Treachery.Cards.TracesToNowhere.ApproachingShadow (approachingShadow) where

import Arkham.Ability
import Arkham.Enemy.Creation
import Arkham.Helpers.Window (DrawnCard (..), drawnCard)
import Arkham.Keyword (Keyword (Aloof))
import Arkham.Matcher
import Arkham.Treachery.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ApproachingShadow = ApproachingShadow TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

approachingShadow :: TreacheryCard ApproachingShadow
approachingShadow = treachery ApproachingShadow Cards.approachingShadow

instance HasAbilities ApproachingShadow where
  getAbilities (ApproachingShadow a) =
    [ restricted a 1 (InThreatAreaOf You)
        $ forced
        $ DrawCard #when You (basic $ #enemy <> NotCard (CardWithKeyword Aloof)) EncounterDeck
    , skillTestAbility $ restricted a 2 (InThreatAreaOf You) actionAbility
    ]

instance RunMessage ApproachingShadow where
  runMessage msg t@(ApproachingShadow attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 (drawnCard -> drawn) _ -> do
      enemy <- createEnemy drawn.card (SpawnEngagedWith iid)
      initiateEnemyAttack enemy (attrs.ability 1) iid
      toDiscard (attrs.ability 1) attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) iid #willpower (Fixed 3)
      pure t
    PassedThisSkillTest _ (isAbilitySource attrs 2 -> True) -> do
      toDiscard (attrs.ability 2) attrs
      pure t
    _ -> ApproachingShadow <$> liftRunMessage msg attrs
