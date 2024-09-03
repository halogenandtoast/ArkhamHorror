module Arkham.Treachery.Cards.IndescribableApparition (
  indescribableApparition,
  IndescribableApparition (..),
)
where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype IndescribableApparition = IndescribableApparition TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

indescribableApparition :: TreacheryCard IndescribableApparition
indescribableApparition = treachery IndescribableApparition Cards.indescribableApparition

instance HasModifiersFor IndescribableApparition where
  getModifiersFor (InvestigatorTarget iid) (IndescribableApparition attrs) | attrs `on` iid = do
    unnamableAtYourLocation <- selectAny $ enemyIs Enemies.theUnnamable <> enemyAtLocationWith iid
    modified
      attrs
      [SkillModifier skillType (-1) | unnamableAtYourLocation, skillType <- allSkills]
  getModifiersFor _ _ = pure []

instance HasAbilities IndescribableApparition where
  getAbilities (IndescribableApparition a) = [restrictedAbility a 1 OnSameLocation $ ActionAbility [] $ ActionCost 2]

instance RunMessage IndescribableApparition where
  runMessage msg t@(IndescribableApparition attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> IndescribableApparition <$> liftRunMessage msg attrs
