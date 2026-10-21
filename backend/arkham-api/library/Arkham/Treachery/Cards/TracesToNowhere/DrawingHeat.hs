module Arkham.Treachery.Cards.TracesToNowhere.DrawingHeat (drawingHeat) where

import Arkham.Ability
import Arkham.Helpers.Location (withLocationOf)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Treachery.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Treachery.Import.Lifted
import Arkham.Window (Window)
import Arkham.Window qualified as Window

newtype DrawingHeat = DrawingHeat TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drawingHeat :: TreacheryCard DrawingHeat
drawingHeat = treachery DrawingHeat Cards.drawingHeat

instance HasAbilities DrawingHeat where
  getAbilities (DrawingHeat a) =
    [ restricted a 1 (InThreatAreaOf You) $ forced $ Leaves #after You Anywhere
    , skillTestAbility $ restricted a 2 (InThreatAreaOf You) actionAbility
    ]

-- | The location left behind, from the @Leaves@ window this ability triggered on.
leftLocation :: [Window] -> Maybe LocationId
leftLocation ws = listToMaybe [lid | (Window.windowType -> Window.Leaving _ lid) <- ws]

instance RunMessage DrawingHeat where
  runMessage msg t@(DrawingHeat attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 (leftLocation -> Just lid) _ -> do
      enemies <- select $ enemyAt lid <> UnengagedEnemy <> not_ EliteEnemy
      withLocationOf iid \destination -> for_ enemies \enemy -> do
        readyThis enemy
        enemyMoveTo (attrs.ability 1) enemy destination
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      chooseOneM iid $ withI18n do
        for_ [#willpower, #agility] \skill ->
          skillVar skill $ labeled "useSkill" $ beginSkillTest sid iid (attrs.ability 2) iid skill (Fixed 4)
      pure t
    PassedThisSkillTest _ (isAbilitySource attrs 2 -> True) -> do
      toDiscard (attrs.ability 2) attrs
      pure t
    _ -> DrawingHeat <$> liftRunMessage msg attrs
