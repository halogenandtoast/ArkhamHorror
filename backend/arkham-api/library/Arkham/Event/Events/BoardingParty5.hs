module Arkham.Event.Events.BoardingParty5 (boardingParty5) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Modifier

newtype BoardingParty5 = BoardingParty5 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

boardingParty5 :: EventCard BoardingParty5
boardingParty5 = event BoardingParty5 Cards.boardingParty5

{- | The printed attack. "Resolve this ability a second time" re-resolves the
whole thing, so the repeat gets the same +3 [combat] and +2 damage.
-}
boardingAttack :: ReverseQueue m => EventAttrs -> InvestigatorId -> m SkillTestId
boardingAttack attrs iid = do
  sid <- getRandom
  skillTestModifiers sid attrs iid [SkillModifier #combat 3, DamageDealt 2]
  chooseFightEnemy sid iid attrs
  pure sid

instance RunMessage BoardingParty5 where
  runMessage msg e@(BoardingParty5 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      sid <- boardingAttack attrs iid
      {- "After this attack resolves" is not conditional on hitting, so both
      outcomes arm it. These two effects are the one-shot guard: they are keyed
      to THIS test, and the repeat below arms nothing, so "a second time" cannot
      chain a third. -}
      onSucceedByEffect sid (atLeast 0) attrs sid $ doStep 1 msg
      onFailedByEffect sid (atLeast 0) attrs sid $ doStep 1 msg
      pure e
    DoStep 1 msg'@(PlayThisEvent iid (is attrs -> True)) -> do
      {- Deferred past the test rather than resolved here: at this point the
      results are known but the attack has not finished, and
      'afterThisTestResolves' can only anchor on the EndSkillTestWindow that is
      still queued. -}
      withSkillTest \sid -> afterThisTestResolves sid do
        allies <- select $ assetControlledBy iid <> #ally <> AssetReady
        unless (null allies) do
          chooseOneM iid $ cardI18n $ scope "boardingParty5" do
            targets allies \aid -> do
              exhaustWith attrs aid
              doStep 2 msg'
            labeled "doNotAttackAgain" nothing
      pure e
    DoStep 2 (PlayThisEvent iid (is attrs -> True)) -> do
      void $ boardingAttack attrs iid
      pure e
    _ -> BoardingParty5 <$> liftRunMessage msg attrs
