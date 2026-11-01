module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Treacheries.BarricadedDoor (barricadedDoor) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Helpers.Modifiers (ModifierType (AlternateSuccessfullInvestigation))
import Arkham.Helpers.SkillTest (getSkillTestTarget, withSkillTest)
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Treachery.Import.Lifted

newtype BarricadedDoor = BarricadedDoor TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

barricadedDoor :: TreacheryCard BarricadedDoor
barricadedDoor = treachery BarricadedDoor Cards.barricadedDoor

{- | "Instead of discovering clues" is a replacement effect, so the Forced lands at
'#when': 'resolveSuccessfulInvestigation' reads 'AlternateSuccessfullInvestigation'
off the location before it queues the discover, and the redirected 'Successful'
then carries the investigator who investigated.
-}
instance HasAbilities BarricadedDoor where
  getAbilities (BarricadedDoor a) = case a.placement of
    AttachedToLocation lid ->
      [ mkAbility a 1 $ forced $ SuccessfulInvestigation #when You (LocationWithId lid)
      , restricted a 2 OnSameLocation $ ActionAbility mempty Nothing $ ActionCost 2
      ]
    _ -> []

instance RunMessage BarricadedDoor where
  runMessage msg t@(BarricadedDoor attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      selectWhenNotNull
        (LocationWithMostClues $ LocationWithoutTreachery (TreacheryAttachedToLocation Anywhere))
        \locations -> chooseOrRunOneM iid $ targets locations $ attachTreachery attrs
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      whenJustM getSkillTestTarget \target ->
        withSkillTest \sid ->
          for_ (individualInvestigationTargets target) \target' ->
            skillTestModifier sid (attrs.ability 1) target'
              $ AlternateSuccessfullInvestigation (toTarget attrs)
      pure t
    Successful (Action.Investigate, _) iid _ (isTarget attrs -> True) _ -> do
      toDiscard (attrs.ability 1) attrs
      assignDamage iid (attrs.ability 1) 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> BarricadedDoor <$> liftRunMessage msg attrs

individualInvestigationTargets :: Target -> [Target]
individualInvestigationTargets = \case
  BothTarget left right -> individualInvestigationTargets left <> individualInvestigationTargets right
  target -> [target]
