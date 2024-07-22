module Arkham.Treachery.Cards.SickeningWebs (sickeningWebs, SickeningWebs (..)) where

import Arkham.Ability
import Arkham.Classes
import Arkham.Investigator.Types (Field (..))
import Arkham.Keyword (Keyword (Alert, Retaliate))
import Arkham.Matcher
import Arkham.Message
import Arkham.Prelude
import Arkham.Projection
import Arkham.Trait (Trait (Spider))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype SickeningWebs = SickeningWebs TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sickeningWebs :: TreacheryCard SickeningWebs
sickeningWebs = treachery SickeningWebs Cards.sickeningWebs

instance HasModifiersFor SickeningWebs where
  getModifiersFor (EnemyTarget eid) (SickeningWebs attrs) = do
    isSpider <- eid <=~> (EnemyWithTrait Spider <> at_ (locationWithTreachery attrs))
    pure $ toModifiers attrs $ guard isSpider *> [AddKeyword Retaliate, AddKeyword Alert]
  getModifiersFor (InvestigatorTarget iid) (SickeningWebs attrs) = do
    here <- iid <=~> InvestigatorAt (locationWithTreachery attrs)
    pure $ toModifiers attrs [CannotMove | here]
  getModifiersFor _ _ = pure []

instance HasAbilities SickeningWebs where
  getAbilities (SickeningWebs x) = [restrictedAbility x 1 OnSameLocation actionAbility]

instance RunMessage SickeningWebs where
  runMessage msg t@(SickeningWebs attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      mlid <- field InvestigatorLocation iid
      for_ mlid $ push . attachTreachery attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      player <- getPlayer iid
      sid <- getRandom
      push
        $ chooseOne
          player
          [ SkillLabel sType [beginSkillTest sid iid (attrs.ability 1) iid sType (Fixed 3)]
          | sType <- [#combat, #agility]
          ]
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      push $ toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> SickeningWebs <$> runMessage msg attrs
