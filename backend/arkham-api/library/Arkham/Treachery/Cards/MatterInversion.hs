module Arkham.Treachery.Cards.MatterInversion (matterInversion) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), getModifiers, modified_)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Trait (Trait (Outsider))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted hiding (EnemyEvaded)

newtype MatterInversion = MatterInversion TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

matterInversion :: TreacheryCard MatterInversion
matterInversion = treachery MatterInversion Cards.matterInversion

instance HasModifiersFor MatterInversion where
  getModifiersFor (MatterInversion attrs) = case attrs.placement of
    AttachedToEnemy eid ->
      modified_
        attrs
        eid
        [RemoveKeyword Keyword.Aloof, AddKeyword Keyword.Retaliate, EnemyFight 1, EnemyEvade 1]
    _ -> pure mempty

instance HasAbilities MatterInversion where
  getAbilities (MatterInversion a) =
    [mkAbility a 1 $ forced $ EnemyEvaded #after You (EnemyWithAttachedTreachery (be a))]

instance RunMessage MatterInversion where
  runMessage msg t@(MatterInversion attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      enemies <-
        select
          $ EnemyWithTrait Outsider
          <> not_ (EnemyWithAttachedTreachery $ treacheryIs Cards.matterInversion)
      if null enemies
        then gainSurge attrs
        else chooseTargetM iid enemies \e -> attachTreachery attrs e >> enemyCheckEngagement e
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      mods <- getModifiers iid
      hollows <- traverse fetchCard [cardId | Hollow cardId <- mods]
      focusCards hollows $ chooseTargetM iid hollows $ drawCard iid
      pure t
    _ -> MatterInversion <$> liftRunMessage msg attrs
