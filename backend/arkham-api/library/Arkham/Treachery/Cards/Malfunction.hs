module Arkham.Treachery.Cards.Malfunction (malfunction, Malfunction (..)) where

import Arkham.Helpers.Investigator
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Vehicle))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Malfunction = Malfunction TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
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
      _ -> pure mempty

instance RunMessage Malfunction where
  runMessage msg t@(Malfunction attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      withLocationOf iid \lid -> do
        vehicles <- select $ ClosestAsset lid (AssetWithTrait Vehicle <> StoryAsset)
        chooseTargetM iid vehicles $ attachTreachery attrs
      pure t
    _ -> Malfunction <$> liftRunMessage msg attrs
