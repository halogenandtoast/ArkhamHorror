module Arkham.Treachery.Cards.Malfunction (malfunction) where

import Arkham.Ability
import Arkham.Helpers.Location
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Vehicle))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Malfunction = Malfunction TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

malfunction :: TreacheryCard Malfunction
malfunction = treachery Malfunction Cards.malfunction

instance HasModifiersFor Malfunction where
  getModifiersFor (Malfunction a) = do
    case a.attached of
      Just (AssetTarget aid) ->
        modifySelect
          a
          Anyone
          [CannotTriggerAbilityMatching (AbilityOnAsset (AssetWithId aid) <> AbilityIsActionAbility)]
      _ -> pure ()

instance HasAbilities Malfunction where
  getAbilities (Malfunction a) =
    guard (isJust a.attached) *> [restricted a 1 OnSameLocation actionAbility]

instance RunMessage Malfunction where
  runMessage msg t@(Malfunction attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      withLocationOf iid \lid -> do
        vehicles <- select $ ClosestAsset lid (AssetWithTrait Vehicle <> StoryAsset)
        chooseTargetM iid vehicles $ attachTreachery attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #intellect (Fixed 3)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> Malfunction <$> liftRunMessage msg attrs
