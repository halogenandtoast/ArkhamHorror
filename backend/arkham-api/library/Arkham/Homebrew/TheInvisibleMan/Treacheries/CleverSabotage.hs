module Arkham.Homebrew.TheInvisibleMan.Treacheries.CleverSabotage (cleverSabotage) where

import Arkham.Ability
import Arkham.Homebrew.TheInvisibleMan.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.TheInvisibleMan.Helpers
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted

newtype CleverSabotage = CleverSabotage TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cleverSabotage :: TreacheryCard CleverSabotage
cleverSabotage = treachery CleverSabotage Cards.cleverSabotage

instance HasAbilities CleverSabotage where
  getAbilities (CleverSabotage a) =
    [ scenarioI18n
        $ withI18nTooltip "cleverSabotage.sabotage"
        $ restricted a 1 (thisExists a $ TreacheryAttachedToAsset (AssetControlledBy You))
        $ forced
        $ SkillTestResult #after You AnySkillTest (FailureResult $ atLeast 2)
    , scenarioI18n
        $ withI18nTooltip "cleverSabotage.discard"
        $ restricted a 2 OnSameLocation doubleActionAbility
    ]

instance RunMessage CleverSabotage where
  runMessage msg t@(CleverSabotage attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      items <- select $ #item <> assetControlledBy iid
      if null items
        then gainSurge attrs
        else chooseOneM iid $ targets items $ attachTreachery attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      for_ attrs.attached.asset $ toDiscardBy iid (attrs.ability 1)
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> CleverSabotage <$> liftRunMessage msg attrs
